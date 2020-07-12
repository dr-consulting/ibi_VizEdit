#' Internal utility that aggregates data and settings needed to trigger an imputation model run
#' 
#' This could use a considerable refactor - need to back through and break up a lot of these complicated functions
#' @param iter number of iterations per chain
#' @param warmup number of warmup interations
#' @param adapt_delta parameter passed to rstan call 
#' @param time_min lower boundary of the time window for imputation 
#' @param time_max upper boundary of the time window for imputation
#' @param ppg_data the processed PPG data.frame be used in the analysis
#' @param ibi_data the IBI data used to define priors for the imputation model
#' @param respiration_cat the population age group used to define the respiration range 
#' @param ds the downsampled sampling rate for the PPG data
#' @param selected_ibis IBI values that will be used in setting the priors (defined by a combination of the expansion
#' factor and the individual respiration rate. 
#' @param ppg_col column name in \code{ppg_data} that contains the PPG signal data. Default is "PPG".
#' @param ibi_col column name in \code{ibi_data} that contains the IBI series 
#' @param time_col defaults to "Time" and represents the synchronized time values for the \code{ppg_data} and 
#' \code{ibi_data}
#' @param expansion_factor is the number of respiration cycles to use in setting the data used to define the paramters
#' in the imputation model 
#' 
#' @return applies a series of helper function and returns a \code{list} of values that are used to define the Gaussian
#' process imputation model. 
#' @export

gp_impute_driver <- function(iter=NULL, warmup=NULL, adapt_delta=NULL, time_min=NULL,time_max=NULL, ppg_data=NULL,
                             ibi_data=NULL, respiration_cat=NULL, ds=NULL, selected_ibis=NULL, ppg_col="PPG",
                             ibi_col="IBI", time_col="Time", expansion_factor=3){

  total_time <- SUMMARY_STATS[["mean_resp"]]*expansion_factor*2
  respiration_stats <- estimate_avg_respiration(ibi_data, respiration_cat, ds, AVERAGE_RESPIRATION_BY_AGE)
  imputation_input_windows <- generate_imputation_input_windows(ppg_data[time_col], total_time, time_min, time_max)
  ppg_inputs <- generate_model_ppg_inputs(time_min, time_max, ppg_data, total_time, ds, imputation_input_windows)
  imputation_window_time_var <- generate_imputation_time(ppg_data, time_min, time_max, ds)
  local_HP_stats <- extract_valid_local_HP_stats(ibi_data, time_min, time_max, selected_ibis, imputation_input_windows)

  driver <- list()
  if(iter<=warmup){
    warning(paste("The number of total iterations must exceed the number of warmup iterations.", "\n",
                  "Total iterations setting:", iter, "\n",
                  "Warmup iterations setting:", warmup))
  }

  driver$iter <- iter
  driver$warmup <- warmup
  driver$adapt_delta <- adapt_delta
  driver$prediction_window <- c(time_min, time_max)
  driver$ds <- ds
  # Initializing stan data list
  driver$gp_data <- list()
  driver$gp_data$mu_R <- respiration_stats$mean
  driver$gp_data$sigma_R <- respiration_stats$sd
  driver$gp_data$mu_HR <- local_HP_stats$mean
  driver$gp_data$sigma_HR <- local_HP_stats$sd
  driver$gp_data$N1 <- nrow(ppg_inputs)
  driver$gp_data$X <- ppg_inputs$Time
  driver$gp_data$Y <- ppg_inputs$PPG
  driver$gp_data$Xp <- imputation_window_time_var
  driver$gp_data$N2 <- length(imputation_window_time_var)

  return(driver)
}


#' Internal \code{ibiVizEdit} utility for running a Bayesian Gaussian process imputation model
#' 
#' @param gp_driver \code{list} returned from \code{ibiVizEdit::gp_impute_driver} that includes all of the values 
#' required for the imputation model. 
#' @param imputation_model a string that defines a \code{Stan} model 
#' 
#' @importFrom parallel detectCores
#' @importFrom rstan rstan_options stan extract traceplot
#' 
#' @return Returns the outputs of a Bayesian imputation model as defined by \code{imputation_model}
#' @export

run_bayesian_gp <- function(gp_driver, imputation_model){
  pars_to_monitor <- c('HR','R', 'Ypred', paste0('a',1:3), paste0('r',1:5))
  options(mc.cores=detectCores()-1)
  rstan_options(auto_write=TRUE)

  init_vals = list(mu_HR = gp_driver$gp_data$mu_HR,
                   mu_R = gp_driver$gp_data$mu_HR)

  model_start_time <- Sys.time()
  imputation_fit <- stan(model_name="Bayesian_GP_imputation", model_code=imputation_model, data=gp_driver$gp_data,
                         pars=pars_to_monitor, warmup=gp_driver$warmup, iter=gp_driver$iter, refresh=100, chains=3,
                         init = list(init_vals, init_vals, init_vals), control=list(adapt_delta=gp_driver$adapt_delta,
                                                                                    max_treedepth=12))
  run_time <- Sys.time() - model_start_time
  units(run_time) <- 'mins'
  run_time <- round(run_time, 2)

  model_pars_summary <- summary(imputation_fit, pars=pars_to_monitor[pars_to_monitor!="Ypred"])$summary

  model_outputs <- list()
  for(p in 1:length(pars_to_monitor)){
    model_outputs[pars_to_monitor[p]] <- extract(imputation_fit, pars_to_monitor[p])
  }
  model_outputs$run_time_mins <- run_time
  model_outputs$fitted_model <- imputation_fit
  model_outputs$model_pars_summary <- model_pars_summary
  model_outputs <- add_MAP_summaries(model_outputs)
  model_output$imputed_df <- generate_gp_ppg_predictions(model_outputs, gp_driver)
  model_output$rhat_warning <- check_for_large_rhats(model_outputs$model_pars_summary)

  return(model_outputs)
}


#' Internal \code{ibiVizEdit} function for replacing corrupted data with imputed data
#' 
#' @param ppg_out PLACEHOLDER
#' @param model_outputs list returned from run_bayesian_gp. 
#' @param ppg_col the column name in the PPG \code{data.frame} that contains the PPG. 
#' @param time_col the column name in the PPG \code{data.frame} that contains the time values. 
#' 
#' @return the ppg_data with the imputed values overwriting the original values
#' @export 

replace_w_imputed <- function(ppg_out=NULL, model_outputs=NULL, ppg_col="PPG", time_col="Time"){
  ppg_out[ppg_col][ppg_out[time_col] %in% model_outputs$imputed_df[time_col]] <- model_outputs$imputed_df[ppg_col]

  return(ppg_out)
}

