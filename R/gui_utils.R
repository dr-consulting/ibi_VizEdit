#' Shiny reactive object.
#'
#' \code{out_text_dir} is an internal utility that prints either a warning that a working directory has not yet been
#' selected or the file path of the working directory selected directly in the UI.
#'
#' @param input_dir is of type \code{character} and represents the working directory passed by the user via the GUI
#'

out_text_dir <- function(input_dir=NULL){
  text <- paste("Directory:", "WARNING - no directory selected!")
  if(!is.null(input_dir)){
    text <- paste("Directory:", input_dir)
  }
  return(text)
}


#' Shiny reactive object.
#'
#' \code{out_text_ppg_file} is an internal utility that prints either a warning that a raw PPG file has not yet been
#' selected or the file path of the PPG file selected directly in the UI.
#'
#' @param input_file is of type \code{character} and represents the raw PPG filename passed by the user via the GUI
#'

out_text_ppg_file <- function(input_file=NULL){
  text <- paste("Directory:", "WARNING - no raw file selected!")
  if(!is.null(input_file)){
    text <- paste("File Path:", input_file)
  }
  return(text)
}


#' Shiny reactive object.
#'
#' \code{out_text_time_file} is an internal utility prints either a warning that a raw timing file has not yet been
#' selected or the file path of the timing file directly in the UI.
#'
#' @param input_file is of type \code{character} and represents the event timing filename passed by the user via the GUI

out_text_time_file <- function(input_file=NULL){
  text <- paste("Directory:", "WARNING - no timing file selected!")
  if(!is.null(input_file)){
    text <- paste("File Path:", input_file)
  }
  return(text)
}
