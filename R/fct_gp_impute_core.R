#' Internal utility for generating imputation input data
#'
#' \code{generate_model_ppg_inputs} is used to generate a \code{data.frame} that contains an appropriately down-sampled
#' set of "Time" and "PPG" inputs for the imputation model. These inputs represent PPG data that "surrounds" the window
#' selected for imputation. The size of the window is dynamically determined for each participant based on average
#' respiration rate (derived from \code{estimate_avg_respiration()}).
#' 
#' @param time_min lower boundary of the time window that defines the PPG data selected for imputation 
#' @param time_max upper boundary of the time window that defines the PPG data selected for imputation
#' @param ppg_data \code{data.frame} that contains the PPG data 
#' @param ds downsampled sampling rate in Hz 
#' @param input_windows a \code{list} that contains the pre and post timing boundaries for the input data used in the 
#' imputation model 
#' @param ppg_col name of the column in the PPG \code{data.frame} that contains the PPG signal
#' @param time_col name of the column in the PPG \code{data.frame} that contains timing information in seconds
#'
#' @return a set of inputs for the imputation model 
#' @importFrom dplyr between
#' @export

generate_model_ppg_inputs <- function(time_min=NULL, time_max=NULL, ppg_data=NULL, ds=NULL,
                                      input_windows=NULL, ppg_col="PPG", time_col="Time"){

  sample_rate <- round(ds/10)

  # Creating a basic set of guardrails here to propagate forward presence/effects of NULL values
  if(!is.null(input_windows$pre) & !is.null(input_windows$post)){
    time_pre <- ppg_data[[time_col]][between(ppg_data[[time_col]], input_windows$pre[1], input_windows$pre[2])]
    time_post <- ppg_data[[time_col]][between(ppg_data[[time_col]], input_windows$post[1], input_windows$post[2])]
    ppg_pre <- ppg_data[[ppg_col]][between(ppg_data[[time_col]], input_windows$pre[1], input_windows$pre[2])]
    ppg_post <- ppg_data[[ppg_col]][between(ppg_data[[time_col]], input_windows$post[1], input_windows$post[2])]
  }

  # Enforcing guardrails for processing steps that could return NULLs as invalid processing outputs
  if(exists('time_pre') & exists('ppg_pre')){
    select_seq <- seq(1, length(time_pre), by=sample_rate)
    time_pre <- time_pre[select_seq]
    ppg_pre <- ppg_pre[select_seq]
  }
  
  # Enforcing guardrails for processing steps that could return NULLs as invalid processing outputs
  if(exists("time_post") & exists("ppg_post")){
    select_seq <- seq(1, length(time_post), by=sample_rate)
    time_post <- time_post[select_seq]
    ppg_post <- ppg_post[select_seq]
  }
  
  # Return an empty object if there is an error in this processing step - will be govern display of user warnings
  if(is.null(time_pre) | is.null(time_post)){
    inputs_df <- NULL
  }
  else{
    inputs_df <- data.frame(Time=c(time_pre, time_post), PPG=c(ppg_pre, ppg_post))
  }

  return(inputs_df)
}


#' Internal utility for maximizing imputation data range
#'
#' \code{generate_imputation_input_windows} is designed to take in the time vector for the entire PPG series, and return
#' a list containing the boundaries that define input data that will be used in the imputation model. The total amount
#' of input data is defined by the \code{expansion_factor}, which is the number of total respiration cycles. The value
#' in \code{ibiVizEdit} defaults to 3, and in the case of imputation models. Combined with the average respiration rate
#' extracted from the PPG and IBI series, the \code{total_input_time} parameter represents the ideal of being able to
#' use approximately three breaths worth of data before the window targeted for imputation and three breaths worth of
#' data after the target window. When the target window approaches the beginning or end of a file, this function will
#' distribute the total time around the target window as evenly as possible. The goal is to come as close as possible to
#' the total amount of input data covering the amount of time defined by \code{total_input_time}.
#'
#' @param time_vector is a \code{numeric} vector that measures the passage of time for the entire PPG signal
#' @param total_input_time is a \code{numeric} 1D vector based on the average number of seconds between breaths
#' (derived) from a statistical analysis of the PPG and IBI signals, adjusting for user-defined age (see
#' \code{estimate_average_respiration()}). This respiration interval length is then multiplied by an "expansion" factor.
#' The default value for this expansion factor in \code{ibiVizEdit} is 3.
#' @param target_time_min is a \code{numeric} 1D vector that contains the minimum time value of the user-defined
#' imputation window.
#' @param target_time_max is a \code{numeric} 1D vector that contains the maximum time value of the user-defined
#' imputation window.
#'
#' @return list that contains the input imputation windows boundaries
#' @export

generate_imputation_input_windows <- function(time_vector, total_input_time, target_time_min, target_time_max){
  time_vector_min <- min(time_vector, na.rm = TRUE)
  time_vector_max <- max(time_vector, na.rm = TRUE)
  pre_bounds <- c(target_time_min - total_input_time/2, target_time_min)
  post_bounds <- c(target_time_max, target_time_max + total_input_time/2)

  # Logic that attempts to balance and maxiimze the amount of input data around the target window.
  if(pre_bounds[1] < time_vector_min | post_bounds[2] > time_vector_max){
    if(pre_bounds[1] < time_vector_min & post_bounds[2] > time_vector_max){
      pre_bounds[1] <- time_vector_min
      post_bounds[2] <- time_vector_max
    }
    else if(pre_bounds[1] < time_vector_min & post_bounds[2] < time_vector_max){
      time_diff <- time_vector_min - pre_bounds[1]
      pre_bounds[1] <- time_vector_min
      max_opts <- c(post_bounds[2] + time_diff, time_vector_max)
      post_bounds[2] <- max_opts[which.min(max_opts)]
    }
    else if(pre_bounds[1] > time_vector_min & post_bounds[2] > time_vector_max){
      time_diff <- post_bounds[2] - time_vector_max
      post_bounds[2] <- time_vector_max
      min_opts <- c(pre_bounds[1] - time_diff, time_vector_min)
      pre_bounds[1] <- min_opts[which.max(min_opts)]
    }
  }

  actual_input_time <- (pre_bounds[2] - pre_bounds[1]) + (post_bounds[2] - post_bounds[1])

  # Generate a simple warning message in the console if the actual differs by 3 seconds or more from desired amount
  if(actual_input_time < total_input_time-3){
    warning("Total input data for imputation model is below the recommended total, based on average respiration.", "\n",
            "Recommended total amount of input data in seconds (pre + post): ", round(total_input_time, 2), "\n",
            "Actual amount of input data in seconds: ", round(actual_input_time, 2))
  }

  # Generate warning messages in the console if the actual amount of data is low, and control whether actual values are
  # returned. The NULL value assignments will be used to contol modal dialog box displays when there is insufficient
  # data for the imputation model.
  if(actual_input_time < 5){
    warning("Input data for imputation model is below an acceptable total. The model cannot generate stable", "\n",
            "estimates with less than 5 seconds of input data. It may be that your file is too small to consider an", "\n",
            "imputation approach.")

    return(list(pre=NULL, post=NULL))
  }
  else if(actual_input_time<10){
    warning("Input data for imputation model is relatively low. The model may not be able to generate stable", "\n",
            "estimates with less than 10 seconds of input data. It may be that your file is too small to consider an", "\n",
            "imputation approach. Inspect results carefully.")

    return(list(pre=pre_bounds, post=post_bounds))
  }
  else{
    return(list(pre=pre_bounds, post=post_bounds))
  }
}


#' Internal utility for extracting local HP mean and sd based on user-identified valid data
#' 
#' @param ibi_data a \code{data.frame} containing the IBI series and corresponding time
#' @param time_min the lower boundary of the imputation window - used to identify the range from which to calculate the 
#' "local" average heart period
#' @param time_max the upper boundary of the imputation window - used to identify the range from which to calculate the
#' "local" average heart period
#' @param selected_points returned from the \code{shiny} brush used to identify the PPG imputation window
#' @param input_windows the windows defined by the user and default settings that will be used by the imputation model 
#' to calculate the replacement PPG data for the targeted selection.
#' @param ibi_col the column name in \code{ibi_data} that contains the IBI time series
#' @param time_col the column name in the \code{ibi_data} that contains the timing information for the IBI series
#' 
#' @return a \code{list} of "local" heart period stats including the mean and standard deviation
#' @export

extract_valid_local_HP_stats <- function(ibi_data=NULL, time_min=NULL, time_max=NULL, selected_points=NULL,
                                         input_windows=NULL, ibi_col="IBI", time_col="Time"){
  pre_valid_ibis <- selected_points$IBI[between(selected_points$Time), input_windows$pre[1], input_windows$pre[2]]
  post_valid_ibis <- selected_points$IBI[between(selected_points$Time), input_windows$post[1], input_windows$post[2]]
  valid_ibis <- c(pre_valid_ibis, post_valid_ibis)
  local_HP_stats <- c(mean=mean(valid_ibis), sd=sd(valid_ibis))

  return(local_HP_stats)
}


#' Internal utility for defining time variable over which to impute
#' 
#' @param ppg_data \code{data.frame} that contains the PPG data 
#' @param time_min lower boundary of the time window that defines the PPG data selected for imputation 
#' @param time_max upper boundary of the time window that defines the PPG data selected for imputation
#' @param ds downsampled sampling rate in Hz 
#' @param time_col name of the column in the PPG \code{data.frame} that contains timing information in seconds
#' 
#' @return the time values corresponding to the imputation window
#' @export

generate_imputation_time <- function(ppg_data=NULL, time_min=NULL, time_max=NULL, ds=NULL, time_col="Time"){
  sample_rate <- round(ds/24)
  imputation_target_time <- ppg_data[time_col][between(ppg_data[time_col], time_min, time_max)]
  imputation_target_time <- imputation_target_time[seq(1, length(imputation_target_time), by=sample_rate)]

  return(imputation_target_time)
}


#' Internal \code{ibiVizEdit} utility for generating diagnostic traceplots
#' 
#' @param model_outputs output object generated from the imputation run
#' @param gp_driver a \code{list} object used to define the imputation problem and includes hyperparameters
#' @param sub_id the subject id defined by the user and used by \code{ibiVizEdit} to label outputs 
#' @param secondary_id a secondary id value defined by the user and used by \code{ibiVizEdit} to label outputs
#' @param study_id as study id value defined by the user and used by \code{ibiVizEdit} to label outputs
#' @param out_dir the internal directory where outputs are stored - created via a default process defined by the 
#' internal settings of \code{ibiVizEdit}
#' 
#' @return saves imputation model diagnostics plots 
#' @importFrom cowplot plot_grid
#' @export

save_gp_imputation_traceplots <- function(model_outputs=NULL, gp_driver=NULL, sub_id=NULL, secondary_id=NULL,
                                          study_id=NULL, out_dir=NULL){
  partial_title <- paste("Parameter Tracelplot: Imputing from", gp_driver$prediction_window[1], "to",
                         gp_driver$prediction_window[2])
  sub_title <- paste("ID:", sub_id, "Time/Task:", secondary_id, "Study ID:", study_id)
  cap_text <- paste("Warmup:", gp_driver$warmup, "Iterations:", gp_driver$iter, "Adapt Delta:", gp_driver$adapt_delta)
  partial_filename <- paste(sub_id, secondary_id, study_id, gp_driver$prediction_window[1], "to",
                            gp_driver$prediction_window[2], gp_driver$iter, gp_driver$warmup, Sys.Date(), sep="_")

  g_HR <- traceplot(model_outputs$fitted_model, pars="HR", inc_warmup=TRUE)+
    labs(title=paste("Heart Rate", partial_title), subtitle=sub_title, caption=cap_text)
  g_R <-  traceplot(model_outputs$fitted_model, pars="R", inc_warmup=TRUE)+
    labs(title=paste("Respiration Rate", partial_title), subtitle=sub_title, caption=cap_text)

  comb_HR_resp_plot <- plot_grid(g_HR, g_R, nrow=2)
  ggsave(filename=paste0(out_dir, "/Comb_HR_R_plot_", partial_filename, ".png"), plot=comb_HR_resp_plot, device="png",
         dpi=300, width=11, height=8, units='in')

  hyper_param_plots <- list()
  hyper_params <- c(paste0("a", 1:3), paste0("r", 1:5))
  for(p in 1:length(hyper_params)){
    hyper_param_plots[hyper_params[p]] <- traceplot(model_outputs$fitted_model, pars=hyper_params[p], inc_warmup=TRUE)+
      labs(title=paste(hyper_params[p], "from", gp_driver$prediction_window[1], "to", gp_driver$prediction_window[2]),
           caption=sub_title)
  }

  comb_hyper_params_plot <- plot_grid(plotlist=hyper_param_plots, nrow=2)
  ggsave(filename=paste0(out_dir, "/Hyperparameters_plot_", partial_filename, ".png"), plot=comb_hyper_params_plot,
         device="png", dpi=300, width=11, height=8, units='in')
}


#' Internal \code{ibiVizEdit} utility for generation a model output summary in .txt
#'
#' @param model_outputs output object generated from the imputation run
#' @param gp_driver a \code{list} object used to define the imputation problem and includes hyperparameters
#' @param sub_id the subject id defined by the user and used by \code{ibiVizEdit} to label outputs 
#' @param secondary_id a secondary id value defined by the user and used by \code{ibiVizEdit} to label outputs
#' @param study_id as study id value defined by the user and used by \code{ibiVizEdit} to label outputs
#' @param out_dir the internal directory where outputs are stored - created via a default process defined by the 
#' internal settings of \code{ibiVizEdit}
#'
#' @return saves imputation model and system information in a raw text file
#' @importFrom benchmarkme get_cpu get_ram
#' @importFrom parallel detectCores
#' @export

save_model_summary_as_text <- function(model_outputs=NULL, gp_driver=NULL,sub_id=NULL, secondary_id=NULL, study_id=NULL,
                                       out_dir=NULL){
  file_name <- paste("/Model_summary", sub_id, secondary_id, study_id, sep="_")
  full_filepath <- paste0(out_dir, file_name, ".txt")

  sink(full_filepath)
  cat(paste0('ID:', '\t\t\t\t', paste(sub_id, secondary_id, study_id, sep = '_')))
  cat(paste0('\nT1:', '\t\t\t\t', round(gp_driver$prediction_window[1], digits = 2)))
  cat(paste0('\nT2:', '\t\t\t\t', round(gp_driver$prediction_window[2], digits = 2)))
  cat(paste0('\nTotal Time', '\t\t\t', round(diff(gp_driver$prediction_window), digits = 2), '(s)'))
  cat(paste0('\nRun Time:', '\t\t\t', model_outputs$run_time, '(mins)'))
  cat(paste0('\nMAP HR:', '\t\t\t\t', round(model_outputs$HR_mode*60, digits = 2)))
  cat(paste0('\nMAP R:', '\t\t\t\t',round(model_outputs$R_mode*60, digits = 2)))
  cat(paste0('\nadapt_delta:', '\t\t\t', gp_driver$adapt_delta))
  cat(paste0('\nIterations:', '\t\t\t', gp_driver$iter))
  cat(paste0('\nWarmup:', '\t\t\t\t', gp_driver$warmup))
  cat('\n\nSystem Information:')
  cat('\n-------------------------------------------------------------------------------------')
  cat(paste0('\nProcessor:', '\t\t\t', get_cpu()$model_name))
  cat(paste0('\nNumber of Cores:', '\t\t', detectCores(logical=F)))
  cat(paste0('\nNumber of Threads:', '\t\t', detectCores(logical=T)))
  cat(paste0('\nRAM:', '\t\t\t\t', paste(round(get_ram()/1073741824), 'GB')))
  cat('\n-------------------------------------------------------------------------------------')
  cat('\n\nGP SUMMARY:\n\n')
  print(model_outputs$model_pars_summary)
  sink()
}


#' Internal \code{ibiVizEdit} utility for appending summary data from an imputation model's posterior distributions
#' 
#' @param model_outputs output object generated from the imputation run
#' @param pars the parameters from the model for which to generate maximum a posteriori estimates
#' 
#' @return appends maximum a posteriori estiamtes from the imputation model to the \code{model_outputs} 
#' @export

add_MAP_summaries <- function(model_outputs=NULL, pars=c("HR", "R")){
  for(p in 1:length(pars)){
    model_outputs[paste0("MAP_", pars[p])] <- estimate_max_density(model_outputs[pars[p]])
  }
  return(model_outputs)
}


#' Internal \code{ibiVizEdit} utility for selecting maximum a posteriori estimates of corrupted PPG data
#'
#' @param model_outputs output object generated from the imputation run
#' @param gp_driver a \code{list} object used to define the imputation problem and includes hyperparameters
#' 
#' @return output \code{data.frame} that upsamples based on the Gaussian process imputation model
#' @importFrom ImputeTS na_kalman 
#' @export

generate_gp_ppg_predictions <- function(model_outputs=NULL, gp_driver=NULL){
  time_df <- data.frame(Time=seq(gp_driver$prediction_window[1], gp_driver$prediction_window[2], by=1/gp_driver$ds))
  pred_df <- data.frame(Time=gp_driver$gp_data$Xp, PPG=colMeans(model_outputs$Ypred))
  pred_df <- merge(time_df, pred_df, by="Time", all=TRUE)

  pred_df$PPG <- na_kalman(pred_df$PPG)
  return(pred_df)
}


#' Internal \code{ibiVizEdit} utility for identying large Rhat estimates in GP imputation model outputs
#' 
#' @param summary_table model summary table that includes rhat values for inspection
#' 
#' @return a warning if one of the r-hats returned from the model is problematic 
#' @export

check_for_large_rhats <- function(summary_table=NULL){
  if(sum(summary_table$Rhat > 1.1) > 1){
    pars_gt_threshold <- row.names(summary_table)[summary_table$Rhat > 1.1]
    rhat_gt_threshold <- summary_table$Rhat[summary_table$Rhat > 1.1]
    problem_pars <- paste("Parameter:", pars_gt_threshold, "Rhat:", rhat_gt_threshold, collapse = "\n")
    msg <- "
    Warning one or more Rhat values exceeded 1.1, an indication that the imputation model did not converge. Check your
    imputation ouputs, traceplots, and model summaries. Visually inspect the imputed PPG outputs in the graphing window.
    Problematic model parameters:
    "
    msg <- paste(str_wrap(msg), problem_pars, sep="\n\n")
    warning(msg)
    return(msg)
  }
  else{
    return(NULL)
  }
}


#' Internal \code{ibiVizEdit} utility for generating the GP imputation model in Stan code
#' 
#' @return the \code{Stan} code that defines the Gaussian process imputation model used by \code{ibiVizEdit}

return_stan_code <- function(){
  model_string <- "
    functions{
    	//covariance function for main portion of the model
    	matrix main_GP(
    		int Nx,
    		vector x,
    		int Ny,
    		vector y,
    		real alpha1,
    		real alpha2,
    		real alpha3,
    		real rho1,
    		real rho2,
    		real rho3,
    		real rho4,
    		real rho5,
    		real HR_f,
    		real R_f){
    			matrix[Nx, Ny] K1;
    			matrix[Nx, Ny] K2;
    			matrix[Nx, Ny] K3;
    			matrix[Nx, Ny] K4;
    			matrix[Nx, Ny] Sigma;

    			//periodic covariance that does not decay
    			for(i in 1:Nx){
    				for (j in 1:Ny){
    					K1[i, j] = alpha1*exp(-square(x[i]-y[j])/2/square(rho1));
    				}
    			}

    			//specifying first quasi-periodic process that incorporates heart rate
    			for(i in 1:Nx){
    				for(j in 1:Ny){
    					K2[i, j] = alpha2*exp(-2*square(sin(pi()*fabs(x[i]-y[j])*HR_f))/square(rho2))*
    					exp(-square(x[i]-y[j])/2/square(rho3));
    				}
    			}

    			//specifying second quasi-periodic process that incorporates heart rate adjusted for respiration
    			for(i in 1:Nx){
    				for(j in 1:Ny){
    					K3[i, j] = alpha3*exp(-2*square(sin(pi()*fabs(x[i]-y[j])*HR_f))/square(rho4))*
    					exp(-2*square(sin(pi()*fabs(x[i]-y[j])*R_f))/square(rho5));
    				}
    			}

    			Sigma = K1+K2+K3;
    			return Sigma;
    		}

    	//function for posterior calculations
    	vector post_pred_rng(
    		real a1,
    		real a2,
    		real a3,
    		real r1,
    		real r2,
    		real r3,
    		real r4,
    		real r5,
    		real HR,
    		real R,
    		real sn,
    		int No,
    		vector xo,
    		int Np,
    		vector xp,
    		vector yobs){
    			matrix[No,No] Ko;
    			matrix[Np,Np] Kp;
    			matrix[No,Np] Kop;
    			matrix[Np,No] Ko_inv_t;
    			vector[Np] mu_p;
    			matrix[Np,Np] Tau;
    			matrix[Np,Np] L2;
    			vector[Np] yp;

    	//--------------------------------------------------------------------
    	//kernel for observed data
    	Ko = main_GP(No, xo, No, xo, a1, a2, a3, r1, r2, r3, r4, r5, HR, R);
    	for(n in 1:No) Ko[n,n] += sn;

    	//--------------------------------------------------------------------
    	//kernel for predicted data
    	Kp = main_GP(Np, xp, Np, xp, a1, a2, a3, r1, r2, r3, r4, r5, HR, R);
    	for(n in 1:Np) Kp[n,n] += sn;

    	//--------------------------------------------------------------------
    	//kernel for observed and predicted cross
    	Kop = main_GP(No, xo, Np, xp, a1, a2, a3, r1, r2, r3, r4, r5, HR, R);

    	//--------------------------------------------------------------------
    	//Algorithm 2.1 of Rassmussen and Williams...
    	Ko_inv_t = Kop'/Ko;
    	mu_p = Ko_inv_t*yobs;
    	Tau=Kp-Ko_inv_t*Kop;
    	L2 = cholesky_decompose(Tau);
    	yp = mu_p + L2*rep_vector(normal_rng(0,1), Np);
    	return yp;
    	}
    }

    data {
    	int<lower=1> N1;
    	int<lower=1> N2;
    	vector[N1] X;
    	vector[N1] Y;
    	vector[N2] Xp;
    	real<lower=0> mu_HR;
    	real<lower=0> mu_R;
    	real<lower=0> sigma_HR;
    	real<lower=0> sigma_R;
    }

    transformed data {
    	vector[N1] mu;
    	for(n in 1:N1) mu[n] = 0;
    }

    parameters {
    	real<lower=0> a1;
    	real<lower=0> a2;
    	real<lower=0> a3;
    	real<lower=0> r1;
    	real<lower=0> r2;
    	real<lower=0> r3;
    	real<lower=0> r4;
    	real<lower=0> r5;
    	real<lower = 0.8333, upper = 3.3333> HR;
    	real<lower = 0.1667, upper = 1> R;
    	real<lower=0> sigma_sq;
    }

    model{
    	matrix[N1,N1] Sigma;
    	matrix[N1,N1] L_S;

    	//using GP function from above
    	Sigma = main_GP(N1, X, N1, X, a1, a2, a3, r1, r2, r3, r4, r5, HR, R);
    	for(n in 1:N1) Sigma[n,n] += sigma_sq;

    	L_S = cholesky_decompose(Sigma);
    	Y ~ multi_normal_cholesky(mu, L_S);

    	//priors for parameters
    	a1 ~ normal(0,2);
    	a2 ~ normal(0,2);
    	a3 ~ normal(0,2);
    	//incorporate minimum and maximum distances - use invgamma
    	r1 ~ inv_gamma(4,4);
    	r2 ~ inv_gamma(4,4);
    	r3 ~ inv_gamma(4,4);
    	r4 ~ inv_gamma(4,4);
    	r5 ~ inv_gamma(4,4);
    	sigma_sq ~ normal(0,2);
    	HR ~ normal(mu_HR,sigma_HR);
    	R ~ normal(mu_R, sigma_R);
    }

    generated quantities {
    	vector[N2] Ypred = post_pred_rng(a1, a2, a3, a4, r1, r2, r3, r4, r5, r6, r7, HR, R, sigma_sq, N1, X, N2, Xp, Y);
    }
    "

  return(model_string)
}
