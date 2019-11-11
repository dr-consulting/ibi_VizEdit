library(shiny)

#' Shiny reactive object.
#'
#' \code{out_text_dir} is an internal utility that provides a means of either printing a warning that a working
#' directory has not been selected or a message specifying the file path of the working directory selected.
#'

out_text_dir <- reactive({
  if(is.null(input$dir)){
    text <- paste("Directory:", "WARNING - no directory selected!")
  }
  else{
    text <- paste("Directory:", rv$out.dir)
  }
  return(text)
})


#' Shiny reactive object.
#'
#' \code{out_text_ppg_file} is an internal utility that provides a means of either printing a warning that a raw PPG file
#' has not been selected or a message specifying the file path of the PPG file selected.
#'

out_text_ppg_file <- reactive({
  if(is.null(input$fileIn)){
    text <- paste("Directory:", "WARNING - no raw file selected!")
  }
  else{
    text <- paste("File Path:", rv$file_name)
  }
  return(text)
})


#' Shiny reactive object.
#'
#' \code{out_text_time_file} is an internal utility that provides a means of either printing a warning that a raw PPG file
#' has not been selected or a message specifying the file path of the PPG file selected.
#'

out_text_time_file <- reactive({
  if(is.null(input$timeIn)){
    text <- paste("Directory:", "WARNING - no timing file selected!")
  }
  else{
    text <- paste("File Path:", rv$time_name)
  }
  return(text)
})
