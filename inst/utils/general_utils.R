#' Finding user home directory.
#'
#' \code{get_user_folder} is designed to identify the home directory of the user who initiated the R session.
#'
#' @export

get_user_folder <- function(input){
  if(Sys.getenv('USERPROFILE')=="")
    user_dir<-"~"
  else if(Sys.getenv("HOMEPATH")!="")
    user_dir<-Sys.getenv('USERPROFILE')
  else
    user_dir<-'C:/'

  shinyDirChoose(input, "wd", roots=c(User=user_dir))
  return(user_dir)
}


#' Setting maximum file size
#'
#' \code{set_file_size_max} is an internal utility that sets the maximum file size for an input data set in megabytes.
#' The default is set to 150 MB. If this is too low in general or for a specific use case, it can be adjusted here.
#'
#' @param size is the total size of the raw file in MB. Default for the program is 500 and can be reset by advanced
#' users
#'
#' @export

set_file_size_max <- function(size = 500){
  options(shiny.maxRequestSize=size*1024^2)
  return(paste(size, "MB"))
}


#' Set min/max to 0/1.
#'
#' \code{range01} is an internal utility that takes a vector of values and returns a new vector with the same
#' distribution, re-scaled to a range of 0 to 1.
#'
#' @export

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

estimate_max_density <- function(x){
  d <- density(x)
  return(d$x[which.max(d$y)])
}


#' Internal utility for \code{ibiVizEdit} that generates a global estimated mean HR
#'
#' @export

estimate_average_HR <- function(ibi_data=NULL, ibi_col="IBI", trim=3){

  ibi_trunc <- ibi_data

  # Drop first and last data points to accomodate end of file issues - defined by trim
  if(!is.null(trim)){
    ibi_trunc <- ibi_data[trim:(nrow(ibi_data)-trim), ]
  }

  if("pnt_type" %in% colnames(ibi_data)){
    ibi_trunc <- ibi_trunc[ibi_trunc[["pnt_type"]] != "Uneditable", ]
  }

  return(1/mean(ibi_trunc[[ibi_col]])*60)
}

#' Internal utility for determing average respiration jointly using PPG and IBI signals
#'
#' @export

estimate_avg_respiration <- function(ibi_data=NULL, respiration_cat=NULL, ds = NULL,
                                     respiration_mapping=AVERAGE_RESPIRATION_BY_AGE, ibi_col="IBI", time_col="Time"){
  respiration_bounds <- respiration_mapping[respiration_cat][[1]]/60/ds

  # Upsampling the IBI signal to enable interpolation for additional processing.
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

  # Attempting to analytically solve for a reasonable respiration priors based on subject file properties
  mean_resp <- (spec_ibi$freq*spec$dens)/sum(spec_ibi$dens)*ds
  wss <- (sum((spec_ibi$freq*ds - mean_resp)*spec_ibi$dens)^2)/sum(spec_ibi$dens)
  sd_resp <- sqrt(wss)

  resp_stats <- c(mean=mean_resp, sd=sd_resp)

  return(resp_stats)
}


#' Internal \code{ibiVizEdit} utility for creating output directory inside working directory
#'
#' @export

create_and_return_output_dir <- function(wd=NULL, case_id=NULL, optional_id=NULL){
  out_folder_name <- paste(case_id, "output", sep="_")

  if(!is.null(optional_id)){
    out_folder_name <- paste(out_folder_name, optional_id, sep = "_")
  }

  out_dir <- paste0(wd, "/", out_folder_name)

  if(!dir.exists(out_dir)){
    dir.create(out_dir)
  }

  return(out_dir)
}


#' Internal \code{ibiVizEdit} utility for creating screenshot directory inside of the output directory
#'
#' @export

create_and_return_screenshot_dir <- function(out_dir=NULL){
  dir <- paste0(out_dir, "/screenshots")

  if(!dir.exists(dir)){
    dir.create(dir)
  }

  return(dir)
}


#' Internal \code{ibiVizEdit} utility for creating an subdirectory for GP imputation model outputs
#'
#' @export

create_and_return_gp_output_subdir <- function(out_dir, gp_driver){
  gp_folder_name <- paste("GP_imputation_output", gp_driver$prediction_window[1], gp_driver$prediction_window[2],
                          Sys.time(), sep="_")
  gp_subdir <- paste0(out_dir, "/", gp_folder_name)

  if(!dir.exists(gp_subdir)){
    dir.create(gp_subdir)
  }

  return(gp_subdir)
}


#' Internal \code{ibiVizEdit} utility that generates a warning if an input expected to be an integer is not
#'
#' @export

raise_not_integer <- function(input_val=NULL, input_name=NULL, lower_bound=NULL, upper_bound=NULL){
  if(input_val %% 1 != 0){
    msg <- "The input value of {input_val} for {input_name} must be an integer"
    if(!is.null(lower_bound) & !is.null(upper_bound)){
      msg <- paste(msg, "between {lower_bound} and {upper_bound}")
    }
    msg <- glue(msg)
    warning(msg)
    return(msg)
  }
}


#' Internal \code{ibiVizEdit} utility that creates a running log of user actions
#'
#' @export

user_action_log <- function(button_name){
  tmp_dat <- data.frame(Timestamp=Sys.time(), action=button_name, stringsAsFactors = FALSE)

  if(is.null(DYNAMIC_DATA[["action_log"]])){
    DYNAMIC_DATA[["action_log"]] <- tmp_dat
  }

  else{
    DYNAMIC_DATA[["action_log"]] <- rbind(DYNAMIC_DATA[["action_log"]], tmp_dat)
  }
}
