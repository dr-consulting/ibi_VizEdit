#' Integrated function for generating interbeat interval time-series.
#'
#' \code{find_ibis} identifies the timing of heart beats in photoplethysmogram data. Input is a time-series of cardiac
#' data generated via recordings from photoplethysmogram sensors. The function does three main things: optimizes
#' bandwidth used in the peak detection algorithm, uses the peaks as proxies for the timing of heart beats, and returns
#' a time-series of interbeat intervals with basic diagnostic statistics.
#'
#' @param x a vector of signal values collected from a photopleythysmogram sensor. If not a vector of values must be an
#' object that can be safely cast to a single vector of temporally sequenced numeric values.
#' @param ds the desired sampling rate, in Hz, for the data used in the peak detection algorithm.
#'
#' @return Returns a list containing a \code{dataframe} of interbeat intervals and their timing and a \code{dataframe}
#' of diagnostic information about the peak detection algorithm optimization parameters and summary statistics.
#'
#' @export
#'

find_ibis <- function(x, ds){
  require(psych)
  x.smooth <- as.numeric(smooth(x))
  x.smooth <- na.omit(x.smooth)
  TIME <- 0:(length(x.smooth)-1)
  x.smooth <- x.smooth-predict(lm(x.smooth~TIME))
  x.smooth <- smooth.spline(x.smooth, nknots = 10000)$y
  s <- round(seq(round(ds/50), round(ds/2), length.out = peak.iter()))
  Z <- data.frame(rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)),
                  rep(NA, length(s)))
  withProgress(message = 'Finding Peaks', value = 0,{
    for(i in 1:length(s)){
      IBI <- find_peaks(x.smooth, s[i])
      time <- time.sum(IBI)/ds
      Z[i,1] <- s[i]
      Z[i,2] <- sd(time)
      Z[i,3] <- max(time)-min(time)
      Z[i,4] <- rmssd(time)
      Z[i,5] <- mean(acf(time, lag.max = length(time)/20, plot = F)$acf)
      Z[i,6] <- s[i]/ds
      incProgress(1/length(s), detail = paste("Pass", i, 'out of', length(s)))
    }
  })
  colnames(Z) <- c('BW', 'SD', 'Range', 'RMSSD', 'AC', 'BW(s)')
  Z <- Z[order(Z$RMSSD, decreasing = F),]
  IBI.fin <- find_peaks(x.smooth, m=Z[1,1])-1
  IBI.fin <- IBI.fin/ds
  IBI.done <- diff(IBI.fin)
  IBI.comp <- list(IBI.done, Z)
  names(IBI.comp) <- c('IBI.done', 'Z')
  return(IBI.comp)
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
#'

find_peaks <- function (x, bw){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - bw + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + bw + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(nubweric(0))
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
#' @param bw the bandwidth used to define the moving window within which a "peak" is identified.
#'
#' @return Returns the sequentially ordered index values at which each "peak" is detected.
#'
#' @export
#'

sum_rev <- function(x){
  Z<-rep(NA, length(x))
  for(i in 1:length(x)){
    Z[i]<-ifelse(i==1, x[i], sum(x[1:(i-1)])+x[i])
  }
  return(Z)
}
