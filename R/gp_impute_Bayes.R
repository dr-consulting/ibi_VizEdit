AVERAGE_RESPIRATION_BY_AGE <- list(`Young Infant (<1 yr)` = c(30, 60),
                                   `Infant/Toddler (1 to 3 yrs)` =c (24, 40),
                                   `Young Child (3 to 6 yrs)` =c (22, 34),
                                   `Child (6 to 12 yrs)` = c(18, 30),
                                   `Adolescent (12 to 18 yrs)` = c(12, 16),
                                   `Adult (18+ yrs)`= c(12, 20))

#' Internal utility that aggregates data and settings needed to trigger an imputation model run
#'

gp_impute_driver <- function(iter=NULL, warmup=NULL, adapt_delta=NULL, ibi_min=NULL, ibi_max=NULL, time_min=NULL,
                             time_max=NULL, ppg_data=NULL, ppg_col="PPG", ibi_data=NULL, ibi_col="IBI", time_col="Time",
                             expansion_factor=3, respiration_cat=NULL, ds=NULL){
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
                                                      AVERAGE_RESPIRATION_BY_AGE, ds)

}


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

generate_model_ppg_inputs <- function(time_min=NULL, time_max=NULL, ppg_data=NULL, ppg_col="PPG", time_col="Time",
                                      expansion_factor=NULL, respiration_cycle_time=NULL, ds=NULL){
  total_time <- 2*expansion_factor*respiration_cycle_time
  input_bounds_list <- generate_imputation_input_windows(ppg_data[time_col], total_time, timen_min, time_max)
  sample_rate <- round(ds/12)

  # Creating a basic set of guardails here to propagate forward presence/effects of NULL values
  if(!is.null(input_bounds_list$pre) & !is.null(input_bounds_list$post)){
    time_pre <- ppg_data[time_col][between(ppg_data[time_col], input_bounds_list$pre[1], input_bounds_list$pre[2])]
    time_post <- ppg_data[time_col][between(ppg_data[time_col], input_bounds_list$post[1], input_bounds_list$post[2])]
    ppg_pre <- ppg_data[ppg_col][between(ppg_data[time_col], input_bounds_list$pre[1], input_bounds_list$pre[2])]
    ppg_post <- ppg_data[ppg_col][between(ppg_data[time_col], input_bounds_list$post[1], input_bounds_list$post[2])]
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
