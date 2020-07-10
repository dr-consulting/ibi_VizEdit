#' Integrated function for generating interbeat interval time-series.
#'
#' \code{find_ibis} identifies the timing of heart beats in photoplethysmogram data. Input is a time-series of cardiac
#' data generated via recordings from photoplethysmogram sensors. The function does three main things: optimizes
#' bandwidth used in the peak detection algorithm, uses the peaks as proxies for the timing of heart beats, and returns
#' a time-series of interbeat intervals with basic diagnostic statistics.
#'
#' @param ppg_signal a vector of signal values collected from a photopleythysmogram sensor. If not a vector of values 
#' must be an object that can be safely cast to a single vector of temporally sequenced numeric values.
#' @param sampling_rate the desired sampling rate, in Hz, for the data used in the peak detection algorithm.
#' @param min_time the minimum time value of the original time series in seconds
#' @param time_adjust the time value adjustment applied to the ibi_series. Within the \code{ibiVizEdit} app the value
#' defaults to 3, which, in effect removes the first and last three seconds of the returned ibi values as a crude 
#' system for dealing with beginning and end of file issues associated with peak detection. 
#' @param peak_iter a value that specificies the number of iterations for the algorithm to use when attempting to 
#' identify the location of the heartbeat "peaks" 
#' @return Returns a list containing a \code{dataframe} of interbeat intervals and their timing and a \code{dataframe}
#' of diagnostic information about the peak detection algorithm optimization parameters and summary statistics.
#'
#' @export
#'
#' @importFrom magrittr %>% 
#' @importFrom psych rmssd

find_ibis <- function(ppg_signal, sampling_rate, min_time, time_adjust = 3, peak_iter){
  s <- seq(round(sampling_rate/8), round(sampling_rate/2), length.out = peak_iter) %>% 
    round() %>% 
    unique()
  
  Z <- data.frame(rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)))
  for(i in 1:length(s)){
    IBI_pos <- find_peaks(ppg_signal, s[i])
    IBI_vals <- time_sum(IBI_pos)/sampling_rate
    Z[i,1] <- s[i]
    Z[i,2] <- sd(IBI_vals)
    Z[i,3] <- max(IBI_vals)-min(IBI_vals)
    Z[i,4] <- rmssd(IBI_vals)
    Z[i,5] <- mean(acf(IBI_vals, lag.max = length(IBI_vals)/20, plot = FALSE)$acf)
    Z[i,6] <- s[i]/sampling_rate
  }
  colnames(Z) <- c('BW', 'SD', 'Range', 'RMSSD', 'AC', 'BW(s)')
  Z <- Z[order(Z$RMSSD, decreasing = FALSE),]
  IBI_pos <- find_peaks(ppg_signal, bw=Z[1,1])-1
  IBI_time <- IBI_pos/sampling_rate + min_time - time_adjust 
  IBI_vals <- time_sum(IBI_pos)/sampling_rate
  IBI_out <- data.frame(IBI=IBI_vals, Time=IBI_time)
  IBI_comp <- list(IBI_out, Z)
  names(IBI_comp) <- c('IBI_out', 'detection_settings')
  return(IBI_comp)
}


#' General peak detection algorithm.
#'
#' \code{find_peaks} identifies the relative location of maximum values in periodic and quasi-peridodic univariate
#' time-series. This original function was taken from https://github.com/stas-g/findPeaks/master/find_peaks.R.
#'
#' @param x a vector of equal interval values measuring a periodic or quasi-periodic process. If not a vector of values,
#' must be an object that can be safely cast to a single vector of temporally sequenced numeric values.
#' @param bw the bandwidth used to define the moving window within which a "peak" is identified.
#'
#' @return Returns the sequentially ordered index values at which each "peak" is detected.
#'
#' @export

find_peaks <- function (x, bw){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - bw + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + bw + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}


#' Internal utility for creating IBI time-series.
#'
#' \code{sum_rev}
#'
#' @param x a vector of equal interval values measuring a periodic or quasi-periodic process. If not a vector of values,
#' must be an object that can be safely cast to a single vector of temporally sequenced numeric values.
#'
#' @return Returns the sequentially ordered index values at which each "peak" is detected.
#' @noRd

sum_rev <- function(x){
  Z<-rep(NA, length(x))
  for(i in 1:length(x)){
    Z[i]<-ifelse(i==1, x[i], sum(x[1:(i-1)])+x[i])
  }
  return(Z)
}


#' Internal utility that creates IBI values from the find_peaks output
#' @noRd

time_sum<-function(x){
  Z<-rep(NA, length(x))
  for(i in 1:length(x)){
    Z[i]<-ifelse(i==1, x[i], x[i]-x[i-1])
  }
  return(Z)
}
