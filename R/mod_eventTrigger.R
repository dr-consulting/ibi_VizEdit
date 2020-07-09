#' Server-side utility for \code{ibiVizEdit} that enables Event Observation from action buttons inside modules
#'
#' Note that currently the logic on whether a function (func) should run needs to be contained inside that function
#'
#' @export
#' @importFrom shiny observeEvent

eventTriggerMod <- function(input, output, session, input_id=NULL, trigger_items=NULL, trigger_values=NULL,
                            trigger_object=NULL, trigger_id=NULL){
  observeEvent(input[[input_id]], {
    trigger_object[[trigger_id]] <- FALSE
    
    if(trigger_items() == trigger_values){
      trigger_object[[trigger_id]] <- TRUE
    }
  })
}