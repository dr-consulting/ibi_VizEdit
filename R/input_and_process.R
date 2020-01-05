#' Internal utility for loading raw photoplethysmogram (PPG) data
#'
#' \code{load_ppg_data} loads the selected raw PPG data for subsequent processing and editing. Accepts tab-delimited
#' .txt formats only.
#'
#' @param file_name is of type \code{character} and identifies the file path containing the raw PPG signal.
#' @param skip_lines is of type \code{int} and specifies the number of lines in the raw file to skip.
#' @param column is of type \code{int} and specifies the column containing in the raw file containing the desired PPG
#' signal.
#' @param sampling_rate is of type \code{int} and specifies the hardware sampling rate for the raw signal in Hz
#'
#' @return returns a data.frame that contains the pre-processed PPG signal and a corresponding set of time codes,
#' initialized at 0.
#'
#' @export
#'

load_ppg <- function(file_name=NULL, skip_lines=NULL, column=NULL, sampling_rate=NULL){
  if(file.exists(file_name)){
    parsed_file_name <- strsplit(file_name, '.')
    file_extension <- parsed_file_name[[length(parsed_file_name)]]
    if(file_extension != "txt"){
      warning(paste("ibiVizEdit does not support", file_extension, "PPG file formats.", "\n",
                    "Your raw PPG data must be in a tab-delimited .txt file.", "\n",
                    "See documentation for additional details."))
    }
    else{
      ppg_file <- read.table(file = file_name, skip = skip_lines, sep='\t')
      ppg_df <- data.frame(PPG = ppg_file[,column],
                           Time = (0:(nrow(ppg_file)-1))/sampling_rate)
      reutrn(ppg_df)
    }
  }
  else{
    warning(paste(file_name, "\n", "Not found. Check working directory and selected PPG file."))
  }
}

#' Internal utility for loading timing file
#'
#' \code{load_timing_data} loads the selected timing information for the targeted file. The timing file must be
#' formatted in such a way that the first column represents the file ID, and each column matches the start and stop of
#' sequential tasks/conditions of interest. Accepts tab-delimited .txt or .csv formats.
#'
#' @param file_name is of type \code{character} and represents the filepath containing timing information for the PPG
#' file of interest, with the first column being a unique identifier.
#' @param case_id is of type \code{character} and represents the unique identifier for the target PPG file. The value
#' is used to match the correct timing information to the target PPG file.
#'
#' @return returns a vector of time stamps that correspond to the start and stop time codes for tasks/conditions of
#' interest
#'
#' @export
#'

load_timing_data <- function(file_name=NULL, case_id=NULL){
  if(file.exists(file_name) & !is.null(case_id)){
    parsed_file_name <- strsplit(file_name, '.')
    file_extension <- parsed_file_name[[length(parsed_file_name)]]

    timing_data <- NULL
    if(file_extension == 'txt'){
      timing_data <- read.table(file_name, header=TRUE, sep='\t')
    }
    else if(file_extension == "csv"){
      timing_data <- read.csv(file_name)
    }
    else{
      warning(paste("ibiVizEdit does not support", file_extension, "timing file formats", "\n",
                    "See documentation for additional details"))
    }

    if(!is.null(timing_data) & case_id %in% timing_data[,1]){
      time_stamps <- timing_data[timing_data[,1] == case_id,]
      return(time_stamps)
    }
    else{
      warning(paste("Case ID:", case_id, "not found in the selected timing file:", '\n',
                    file_name))
    }
  }
}

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
  IBI_matrix <- cbind(IBI, IBI_time + min(clean_ppg[time_col]))
  colnames(IBI_matrix)<-c('IBI', 'Time')
  return(IBI_matrix)
}

#' Internal utility for ibiVizEdit that centers PPG and IBI timing values a the start of a task file.
#'
#' \code{time_center} takes a PPG and/or IBI file and centers it at the very start of a task file. The effect is to cut
#' off any data prior to the initial time stamp and to set that value to 0, representing the start of the condition or
#' conditions of interest.
#'
#' @param x PPG or IBI \code{data.frame}
#' @param time_col the name of the column in \code{x} that contains the time data
#' @param time_df the name of the \code{vector} that contains timestamps for the initial task and subsequent tasks.
#' Must be formatted such that the element is the start of the first task/condition, the second element is the end of
#' that task, the third is start of the second condition/task, the fourth is the end of that task and so on.
#'
#' @return Returns a re-coded version of the inputs with new timing
#'
#' @export
#'

time_center <- function(x, time_col = 'Time', timing_series = NULL){
  if(!is.null(timing_series)){
    x[time_col] <- x[time_col] - min(timing_series)
  }
  return(x)
}
