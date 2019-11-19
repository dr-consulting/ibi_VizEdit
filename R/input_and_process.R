#' Integrated function for generating IBIs matched with a timing variable
#'
#' \code{generate_ibis_w_timing} integrates other \code{ibi_VizEdit} functions \code{iter_IBI} and \code{sum_rev} to
#' produce a matrix of values. One column stores the returned interbeat intervals. The other column represents a timing
#' variable. Timing and IBIs are estimated to the nearest millisecond.
#'
#' @param clean_ppg is the cleaned and processed photoplethysmogram data (either a \code{data.frame} or a
#' \code{matrix}).
#' @param ppg_col is the name of the column in the \code{clean_ppg} object that contains measuresments of the pulse
#' waveform.
#' @param time_col is the name of the column in the \code{clean_ppg} object that measures the passage of time associated
#' with the pulse waveform.
#'
#' @return Returns a matrix of interbeat intervals and their relative timing.
#'
#' @export
#'

generate_ibis_w_timing <- function(clean_ppg, ppg_col="PPG", time_col="Time"){
  # Generating IBIs:
  IBI_list <- iter_IBI(clean_ppg[ppg_col], ds=1000)
  IBI <- IBI_list$IBI_done
  IBI_time <- sum_rev(IBI)
  IBI_matrix <- cbind(IBI, IBI_time+min(clean_ppg[time_col]))
  colnames(IBI_matrix)<-c('IBI', 'Time')
  return(IBI_matrix)
}

#' Internal utility for ibiVizEdit that centers PPG and IBI timing values a the start of a task file.
#'
#' \code{time_center} DESCRIPTION
#'
#' @param x description
#' @param time_col descirption
#' @param time_file description
#'
#' @return Returns a re-coded version of the inputs with new timing
#'
#' @export
#'

time_center <- function(x, time_col = 'Time', time_file = NULL){
  if(!is.null(time_file)){
    tmp <- x[time_col]
  }
}
