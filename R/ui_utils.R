library(shiny)
#' UI utility for \code{ibiVizEdit} that generates a working directory button:
#'
#' @export

fileButtons <- function(id, label="Select Working Directory", color){
  ns <- NS(id)

  tagList(tags$h2("Select File(s) and Directory:"),
          shinyDirButton(id=ns("wd_in"), label="Select Directory", title="Choose Your Working Directory"),
          tags$br(),
          tags$p(textOutput(ns("wd_out"))),
          shinyFilesButton(id=ns("ppg_in"), label="Choose PPG File", title="Select Raw PPG .txt File", multiple=FALSE),
          tags$br(),
          tags$p(textOutput(ns("ppg_filepath"))),
          shinyFilesButton(id=ns("timing_in"), label="Select Timing File", title="Select (optional) Timing File",
                           multiple=FALSE),
          tags$br(),
          tags$p(textOutput(ns("timing_filepath"))))
}

