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
#' @export
#'

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
            "imputation approach")
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

