#' UI module for \code{ibiVizEdit} that serves as a basic wrapper and enables single actionButton generation.
#'
#' @export
#' @importFrom shiny uiOutput

dynamicClrButtonModUI <- function(id=NULL, inline=FALSE){
  ns <- NS(id)
  uiOutput(ns("rendered_button"), inline=inline)
}


#' Server module for \code{ibiVizEdit} that dynamically switches actionButton UIs based on color
#'
#' @export
#' @importFrom shiny actionButton

dynamicClrButtonMod <- function(input, output, session, status_name=NULL, label=NULL, hotkey=NULL, hotkey_map=NULL,
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
    
    if(!is.null(hotkey) & !is.null(hotkey_map)){
      tags$script(HTML(track_hotkey_presses(key=hotkey, key_map=hotkey_map, button_name=button_name)))
    }
    
    if(!default_display){
      label <- updated_label
      color_arg <- updated_color
    }
    
    actionButton(session$ns(button_name), label=label, style=color_arg)
  })
}