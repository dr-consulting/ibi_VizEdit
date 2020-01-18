#gp_impute_Bayes
AVERAGE_RESPIRATION_BY_AGE <- list(`Young Infant (<1 yr)` = c(30, 60),
                                   `Infant/Toddler (1 to 3 yrs)` =c (24, 40),
                                   `Young Child (3 to 6 yrs)` =c (22, 34),
                                   `Child (6 to 12 yrs)` = c(18, 30),
                                   `Adolescent (12 to 18 yrs)` = c(12, 16),
                                   `Adult (18+ yrs)`= c(12, 20))

gp_impute_driver <- function(iter=NULL, warmup=NULL, adapt_delta=NULL, ibi_min=NULL, ibi_max=NULL, time_min=NULL,
                             time_max=NULL, ppg_data=NULL, ppg_col="PPG", ibi_data=NULL, ibi_col="IBI", time_col="Time",
                             expansion_factor=3, respiration_cat=NULL){
  driver <- list()
  if(iter<=warmup){
    warning(paste("The number of total iterations must exceed the number of warmup iterations.", "\n",
                  "Total iterations setting:", iter, "\n",
                  "Warmup iterations setting:", warmup))
  }
  driver$iter <- iter
  driver$warmup <- warmup
  driver$adapt_delta <- adapt_delta
  driver$ibi_min <- ibi_min
  driver$ibi_max <- ibi_max
  driver$prediction_window <- c(time_min, time_max)
  driver$mean_respiration <- estimate_avg_respiration(ppg_data, ppg_col, ibi_data, ibi_col, respiration_cat,
                                                      AVERAGE_RESPIRATION_BY_AGE)
}


estimate_avg_respiration <- function(ppg_data=NULL, ppg_col=NULL, ibi_data=NULL, ibi_col="IBI", respiration_cat=NULL,
                                     respiration_mapping=AVERAGE_RESPIRATION_BY_AGE, ds=NULL){
  respiration_bounds <- respiration_mapping[respiration_cat][[1]]/60/ds
  spec_ppg <- mvspec(ppg_data[ppg_col], spans=c(7, 7), taper=.1, demean=TRUE, log='no', plot=FALSE)
  spec_ibi <- mvspec(ibi_data[ibi_col], spans=c(7, 7), taper=.1, demean=TRUE, log='no', plot=FALSE)

  # Truncating data to expected respiration frequency range.
  # The more consistent and cleaner the signal across ppg and ibi waverforms, the stronger prior for the average
  spec_ppg <- data.frame(freq = spec_ppg$freq[spec_ppg$freq >= respiration_bounds[1] &
                                                spec_ppg$freq <= respiration_bounds[2]],
                         dens = spec_ppg$spec[spec_ppg$freq >= respiration_bounds[1] &
                                                spec_ppg$freq <= respiration_bounds[2]])

  spec_ibi <- data.frame(freq = spec_ibi$freq[spec_ibi$freq >= respiration_bounds[1] &
                                                spec_ibi$freq <= respiration_bounds[2]],
                         dens = spec_ibi$spec[spec_ibi$freq >= respiration_bounds[1] &
                                                spec_ibi$freq <= respiration_bounds[2]])

  # Taking a weighted average using spectral density weights from each signal as weights of respiration frequency
  # Multiplying back out by ds (the downsampling rate) to return frequency to Hz
  mean_resp <- (spec_ppg$freq*spec_ppg$dens + spec_ibi$freq*spec$dens)/(spec_ppg$dens + spec_ibi$dens)*ds

  return(mean_resp)
}


model_ppg_inputs <- function(time_min=NULL, time_max=NULL, ppg_data=NULL, ppg_col="PPG", ppg_time_col="Time",
                             expansion_factor=NULL){

}
