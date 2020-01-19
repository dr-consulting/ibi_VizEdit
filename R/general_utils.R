#' Finding user home directory.
#'
#' \code{get_user_folder} is designed to identify the home directory of the user who initiated the R session.
#'

get_user_folder <- function(){
  if(Sys.getenv('USERPROFILE')=="")
    user_folder<-"~"
  else if(Sys.getenv("HOMEPATH")!="")
    user_folder<-Sys.getenv('USERPROFILE')
  else
    user_folder<-'C:/'

  return(user_folder)
}


#' Setting maximum file size
#'
#' \code{set_file_size_max} is an internal utility that sets the maximum file size for an input data set in megabytes.
#' The default is set to 150 MB. If this is too low in general or for a specific use case, it can be adjusted here.
#'
#' @param size is the total size of the raw file in MB. Default for the program is 500 and can be reset by advanced
#' users
#'

set_file_size_max <- function(size = 500){
  options(shiny.maxRequestSize=size*1024^2)
}


#' Initialize global reactive values.
#'
#' \code{ini_global_reactive_values} is an internal utility that initializes a set of global reactive values that affect
#' the behavior of the GUI, including colors, displays, and which actions are available to a user.
#'
#' @param tot_edits is a \code{dataframe} that is used to maintain a running track record unique edits made by a user.
#' @param base_on is used to toggle access to the "base" editing functions on and off. Default is off.
#' @param adv_on is used to toggle access to the "advanced" editing functions on and off. Default is off.
#' @param ppg_on is used to toggle the display of the underlying PPG waveform on and off. Only works when a user is
#' zoomed in on a section of the file. Default is off.
#' @param select_on is used to toggle on access to the select brush on the "base" editing panel. This particular select
#' brush supports the use of the "combine", "divide", and "average" functions. Default is off.
#' @param add_delete_on is used to toggle on access to mouse-click-based "add" and "delete" functionality.
#' @param select_on2 is used to toggle on access to the select brush on the "advanced" editing panel. This particular
#' select brush allows the user to specify the section of corrupted data targeted for imputation. Default is off.
#' @param add_delete_on2 is used to toggle on access to mouse-click-based "add" and "delete" functionality. Default is
#' off.
#' @param start.time is used to store the time and date the user began processing a target file.
#' @param GP_impute_tab is used to store ongoing information about Gaussian imputation attempts. The table is included
#' in the final output.
#'

ini_global_reactive_values <- function(){
  rv_start <- reactiveValues(tot_edits=data.frame(), base_on=0, adv_on=0, ppg_on=0, select_on=0, add_delete_on=0,
                             select_on2=0, add_delete_on2=0, start_time=NULL, GP_impute_tab=NULL, IBI_temp=NULL)
  return(rv_start)
}


#' Set min/max to 0/1.
#'
#' \code{range01} is an internal utility that takes a vector of values and returns a new vector with the same
#' distribution, re-scaled to a range of 0 to 1.
#'

range01 <- function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}


#' Internal utility for estimating mode of posterior distribution from imputation models
#'
#' \code{estimate_mode} takes the posterior samples from a \code{stanfit} object and returns the mode of the posterior
#' distribution
#'
#' @param x a \code{numeric} series, vector, or matrix of values
#'
#' @return maximum a posteriori value of the input object
#'
#' @export

estimate_mode <- function(x){
  d <- density(x)
  return(d$x[which.max(d$y)])
}


#' Internal utility for deteming average respiration jointly using PPG and IBI signals
#'

estimate_avg_respiration <- function(ibi_data=NULL, ibi_col="IBI", time_col="Time", respiration_cat=NULL, ds = NULL,
                                     respiration_mapping=AVERAGE_RESPIRATION_BY_AGE){
  respiration_bounds <- respiration_mapping[respiration_cat][[1]]/60/ds
  time_df <- data.frame(time_col=seq(min(ibi_data[time_col], na.rm = TRUE),
                                     max(ibi_data[time_col], na.rm = TRUE), by = .01))
  ibi_data <- merge(time_df, ibi_data, by=time_col, all=TRUE)
  ibi_data[ibi_col] <- na_kalman(ibi_data[time_col])

  ibi_filtered <- bwfilter(ts(ibi_data[ibi_col], frequency = 100), from = respiration_bounds[1]*ds,
                           to = respiration_bounds[2]*ds, bandpass = TRUE, f = 100)

  spec_ibi <- mvspec(ibi_filtered, spans=c(7, 7), taper=.1, demean=TRUE, log='no', plot=FALSE)

  # Truncating data to expected respiration frequency range.
  # The more consistent and cleaner the signal across ppg and ibi waverforms, the stronger prior for the average
  spec_ibi <- data.frame(freq = spec_ibi$freq[spec_ibi$freq >= respiration_bounds[1] &
                                                spec_ibi$freq <= respiration_bounds[2]],
                         dens = spec_ibi$spec[spec_ibi$freq >= respiration_bounds[1] &
                                                spec_ibi$freq <= respiration_bounds[2]])

  # Taking a weighted average using spectral density weights from each signal as weights of respiration frequency
  # Multiplying back out by ds (the downsampling rate) to return frequency to Hz
  mean_resp <- (spec_ibi$freq*spec$dens)/sum(spec_ibi$dens)*ds

  return(mean_resp)
}

