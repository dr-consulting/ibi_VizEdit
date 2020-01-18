#' Shiny reactive object.
#'
#' \code{out_text_sys_path} is an internal utility that prints either a warning that a working directory has not yet
#' been selected or the file path of the working directory selected directly in the UI.
#'
#' @param input_sys_path is of type \code{character} and represents the working directory passed by the user via the GUI
#'

out_text_sys_path <- function(input_sys_path=NULL){
  text <- paste("Directory:", "WARNING - no directory selected!")
  if(!is.null(input_sys_path)){
    text <- paste("System Path:", input_sys_path)
  }
  return(text)
}
