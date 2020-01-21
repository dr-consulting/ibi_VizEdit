# Will need to have functionality to auto populate fields when existing data are loaded
# Probably want a modal dialog box that says something like "Import Settings"

#' Server-side utility for \code{ibiVizEdit} that dynamically switches actionButton UIs based on color
#'
#' @export

dynamicButtonMod <- function(active=FALSE, label=NULL, input_name=NULL, active_color=BACKGROUND_COLORS["standard"],
                             inactive_color=BACKGROUND_COLORS["inactive"], input, output, session){
  output$rendered_button <- renderUI({
    color_arg <- inactive_color
    if(active) color_arg <- active_color

    actionButton(session$ns(input_name), label=label, style=color_arg)
  })
}

#' Server-side utility for \code{ibiVizEdit} that tracks button status as a set of reactive values
#'
#' @export


#' Server-side utility for \code{ibiVizEdit} that generates a warning if an input expected to be an integer is not
#'
#' @export

raise_not_integer <- function(input_val=NULL, input_name=NULL, lower_bound=NULL, upper_bound=NULL){
  if(input_val %% 1 != 0){
    msg <- "The input value of {input_val} for {input_name} must be an integer"
    if(!is.null(lower_bound) & !is.null(upper_bound)){
      msg <- paste(msg, "between {lower_bound} and {upper_bound}")
    }
    msg <- glue(msg)
    warning(msg)
    return(msg)
  }
}


#' Server-side utility for \code{ibiVizEdit} that updates summary stats displayed in text window
#'
#' @export

headUpInfo <- function(input, output, session){
  req(EDIT_DATA[["edited_IBI"]], SUMMARY_STATS[[c("mean_HR", "mean_R")]])

  temp_point <- reactive({
    nearPoints(EDIT_DATA[["edited_IBI"]][c("IBI", "Time")], coordinfo=input$hover_main, maxpoints=1)
  })

  output$heads_up <- renderPrint({
    cat("Average HR & Resp:\n")
    cat("HR BPM:", "\t\t\t", round(SUMMARY_STATS[["mean_HR"]], 2))
    cat("Resp per min:", "\t", round(SUMMARY_STATS[["mean_R"]], 2))
    cat("\nNear Point:\n")
    temp_point()
  })
}

#' Sever-side utility for \code{ibiVizEdit} that generates a warning when using a function when precondition(s) not met
#'
#' Goal should be to make this as general as possible. Specifically thought of a situation when trying to use the
#' "Restore IBI" but but being on click-select instead of drag-select mode
