#' UI module for \code{ibiVizEdit} that serves as a basic wrapper and enables single actionButton generation.
#' 
#' @param id value for the namespace that links the server and ui components of the module
#' @param inline setting passed on to the ui defining whether the button should be placed in line or not
#' @importFrom shiny uiOutput
#' @noRd

dynamicClrButtonModUI <- function(id=NULL, inline=FALSE){
  ns <- NS(id)
  uiOutput(ns("rendered_button"), inline=inline)
}


#' Server module for \code{ibiVizEdit} that dynamically switches actionButton UIs based on color
#'
#' @param input,output,session internal {shiny} parameters
#' @param status_name a value passed via internal processes of {ibiVizEdit} determining whether a given button is 
#' available for use.
#' @param label the initial label presented on the button rendered in the ui 
#' @param updated_label the updated if there is a change in label that corresponds with a change in status
#' @param default_display_name default name displayed in the {ibiVizEdit} ui
#' @param button_name default set to "click_in" used to define the connection between the server and ui modules
#' @param active_color the active color - defaults to settings defined in {ibiVizEdit} internals
#' @param inactive_color the inactive color - defaults to settings defined in {ibiVizEdit} internals
#' @param updated_color the updated color (when active) - defaults to settings defined in {ibiVizEdit} internals
#' @importFrom shiny actionButton
#' @noRd

dynamicClrButtonMod <- function(input, output, session, status_name=NULL, label=NULL,
                                updated_label=NULL, default_display_name=NULL, button_name="click_in",
                                active_color=BUTTON_COLORS["standard"], inactive_color=BUTTON_COLORS["inactive"],
                                updated_color=BUTTON_COLORS["warning"]){
  
  output$rendered_button <- renderUI({
    active <- as.logical(BUTTON_STATUS[[status_name]])
    default_display <- TRUE
    color_arg <- inactive_color
    
    if(!is.null(default_display_name)){
      default_display <- as.logical(BUTTON_STATUS[[default_display_name]])
    }
    
    if(active){
      color_arg <- active_color
    }
    
    if(!default_display){
      label <- updated_label
      color_arg <- updated_color
    }
    
    actionButton(session$ns(button_name), label=label, style=color_arg)
  })
}
