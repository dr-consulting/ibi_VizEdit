#' UI Shiny Module for \code{ibiVizEdit} that serves as a basic wrapper and enables dynamic updating of checkbox UI
#'
#' @export
#' @importFrom shiny uiOutput

dynamicCheckBoxInputModUI <-function(id=NULL){
  ns <- NS(id)
  uiOutput(ns("rendered_checkbox"))
}

#' Server Shiny Module for \code{ibiVizEdit} that serves as a basic wrapper and enables dynamic updating of checkbox UI
#'
#'
#' @export
#' @importFrom shinyWidgets awesomeCheckboxGroup

dynamicCheckBoxInputMod <- function(input, output, session, label=NULL, choices=NULL, selected=NULL, paste_char=NULL){
  output$rendered_checkbox <- renderUI({
    if(!is.null(paste_char)){
      choices = paste0(choices, paste_char)
      selected = paste0(slected, paste_char)
    }
    tagList(awesomeCheckboxGroup("checbox_in", label=label, choices=choices, selected=selected),
            tags$head(tags$style(HTML("
                                    #checkbox :after, #checkbox :before{
                                    background-color: #426ebd;
                                    }"))))
    
  })
}