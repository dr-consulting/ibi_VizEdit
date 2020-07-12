#' Server-side utility for \code{ibiVizEdit} that enables Event Observation from action buttons inside modules
#'
#' Note that currently the logic on whether a function (func) should run needs to be contained inside that function
#'
#' @param input,output,session 
#' @param input_id id label - actual name stored within the {shiny} {input} object
#' @param trigger_items PLACEHOLDER 
#' @param trigger_values PLACEHOLDER
#' @param trigger_object PLACEHOLDER
#' @param trigger_id PLACEHOLDER
#' @importFrom shiny observeEvent
#' @noRd

eventTriggerMod <- function(input, output, session, input_id=NULL, trigger_items=NULL, trigger_values=NULL,
                            trigger_object=NULL, trigger_id=NULL){
  observeEvent(input[[input_id]], {
    trigger_object[[trigger_id]] <- FALSE
    
    if(trigger_items() == trigger_values){
      trigger_object[[trigger_id]] <- TRUE
    }
  })
}
