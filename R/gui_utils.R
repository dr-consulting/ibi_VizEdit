library(shiny)

#' Shiny reactive object.
#'
#' \code{out_text_dir} is an internal utility that prints either a warning that a working directory has not yet been
#' selected or the file path of the working directory selected directly in the UI.
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
#' \code{out_text_ppg_file} is an internal utility that prints either a warning that a raw PPG file has not yet been
#' selected or the file path of the PPG file selected directly in the UI.
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
#' \code{out_text_time_file} is an internal utility prints either a warning that a raw timing file has not yet been
#' selected or the file path of the timing file directly in the UI.
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
