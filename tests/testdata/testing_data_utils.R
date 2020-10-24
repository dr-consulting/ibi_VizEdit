#' Outputs a cosine waveform as a vector of values. Represents a deterministic analogue of a PPG waveform
#' 
#' @param sampling_rate single integer representing Hz 
#' @param min_time single value representing the lower boundary of the simulated time window
#' @param max_time single value representing the upper boundary of the simulated time window
#' @param hr_freq the simulated heart rate frequency - expressed in beats per second 

create_sim_ppg <- function(sampling_rate, min_time, max_time, hr_freq){ 
  time <- seq(min_time, max_time, by=1/sampling_rate)
  sim_ppg <- cos(2*pi*time*hr_freq)
  df <- data.frame(Time = time, 
                   PPG = sim_ppg)
  return(df)
}
