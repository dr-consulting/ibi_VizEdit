#' Internal utility for generating imputation input data
#'
#' \code{generate_model_ppg_inputs} is used to generate a \code{data.frame} that contains an appropriately down-sampled
#' set of "Time" and "PPG" inputs for the imputation model. These inputs represent PPG data that "surrounds" the window
#' selected for imputation. The size of the window is dynamically determined for each participant based on average
#' respiration rate (derived from \code{estimate_avg_respiration()}).
#'
#' @param time_min is a \code{numeric} 1D vector that contains the minimum time value of the user-defined imputation
#' window.
#' @param time_max is a \code{numeric} 1D vector that contains the maximum time value of the user-definedimputation
#' window.
#' @param ppg_data is a \code{data.frame} that contains the processed PPG signal and a time variable.
#' @param ppg_col is of type \code{character} and is the column name in the \code{ppg_data} that contains the PPG signal
#' @param time_col is of type \code{character} and is the column name in the \code{ppg_data} that contains the time
#' variable
#' @param expansion_factor is a \code{integer} that represents the number of average respiration cycles the program will
#' use when exapanding to select input data for the imputation model.
#' @param respiration_cycle_time is a \code{numeric} value that indicates the average number of seconds elapsed during
#' each respiration cycle. The value is derived from \code{estimate_avg_respiration()}
#' @param ds is a \code{integer} that represents the final "downsampled" rate of the PPG data, in Hz
#'
#' @export
#'

generate_model_ppg_inputs <- function(time_min=NULL, time_max=NULL, ppg_data=NULL, total_time=NULL, ds=NULL,
                                      input_windows=NULL, ppg_col="PPG", time_col="Time"){

  sample_rate <- round(ds/12)

  # Creating a basic set of guardails here to propagate forward presence/effects of NULL values
  if(!is.null(input_windows$pre) & !is.null(input_windows$post)){
    time_pre <- ppg_data[time_col][between(ppg_data[time_col], input_windows$pre[1], input_windows$pre[2])]
    time_post <- ppg_data[time_col][between(ppg_data[time_col], input_windows$post[1], input_windows$post[2])]
    ppg_pre <- ppg_data[ppg_col][between(ppg_data[time_col], input_windows$pre[1], input_windows$pre[2])]
    ppg_post <- ppg_data[ppg_col][between(ppg_data[time_col], input_windows$post[1], input_windows$post[2])]
  }

  # Enforcing guardrails for processing steps that could return NULLs as invalid processing outputs
  if(exists("time_pre") & exists("ppg_pre")){
    if(length(time_pre) == length(ppg_pre)){
      select_seq <- seq(1, length(time_pre), by=sample_rate)
      time_pre <- time_pre[select_seq]
      ppg_pre <- ppg_pre[select_seq]
    }
    else{
      warning("Input time vector and ppg vector do not match in length. Specify your imputation window again.", "\n",
              "If this continues to happen either contact the developer (mbarstead@deadreckoning.consulting), or", "\n",
              "submit an issue on GitHub: https://github.com/matgbar/IBI_VizEdit")
      time_pre <- NULL
      ppg_pre <- NULL
    }
  }

  # Enforcing guardrails for processing steps that could return NULLs as invalid processing outputs
  if(exists("time_post") & exists("ppg_post")){
    if(length(time_post) == length(ppg_post)){
      select_seq <- seq(1, length(time_post), by=sample_rate)
      time_post <- time_post[select_seq]
      ppg_post <- ppg_post[select_seq]
    }
    else{
      warning("Input time vector and ppg vector do not match in length. Specify your imputation window again.", "\n",
              "If this continues to happen either contact the developer (mbarstead@deadreckoning.consulting), or", "\n",
              "submit an issue on GitHub: https://github.com/matgbar/IBI_VizEdit")
      time_post <- NULL
      ppg_post <- NULL
    }
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
#'

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
#'

generate_imputation_time <- function(ppg_data=NULL, time_min=NULL, time_max=NULL, ds=NULL, time_col="Time"){
  sample_rate <- round(ds/24)
  imputation_target_time <- ppg_data[time_col][between(ppg_data[time_col], time_min, time_max)]
  imputation_target_time <- imputation_target_time[seq(1, length(imputation_target_time, by=sample_rate))]
  return(imputation_target_time)
}
