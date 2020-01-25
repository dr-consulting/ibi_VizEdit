# Will need to have functionality to auto populate fields when existing data are loaded
# Probably want a modal dialog box that says something like "Import Settings"

#' Server-side utiltiy for \code{ibiVizEdit} that dynamically updates data entry options on UI, starts with defaults
#'
#' @export

dynamicTextInputMod <- function(input, output, session, label=NULL, value=NULL){
  output$rendered_field <- renderUI({
    textInput("text_in", label=label, value=value)
  })
}

#' Sever-side utility for \code{ibiVizEdit} that dynamically updates data entry options on UI, starts with defaults
#'
#' @export

dynamicNumInputMod <- function(input, output, session, label=NULL, value=NULL){
  output$rendered_field <- renderUI({
    numericInput("numeric_in", label=label, value=value)
  })
}

#' Sever-side utility for \code{ibiVizEdit} that dynamically produces an awesomeCheckboxGroup with pre-selections
#'
#' @export

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

#' Sever-side utility for \code{ibiVizEdit} that dynamically produces a selectInput Field
#'
#' @export


dynamicSelectInputMod <- function(input, output, session, label=NULL, choices=NULL, choice_index=NULL){
  output$rendered_dropdown <- renderUI({
    selectInput("dropdown_in", label=label, choices=names(choices), selected=names(choices)[choice_index])
  })
}

#' Server-side utility for \code{ibiVizEdit} that dynamically switches actionButton UIs based on color
#'
#' @export

dynamicClrButtonMod <- function(input, output, session, active=FALSE, label=NULL,
                                active_color=BUTTON_COLORS["standard"],
                                inactive_color=BUTTON_COLORS["inactive"]){

  output$rendered_button <- renderUI({
    #browser()
    color_arg <- inactive_color
    if(active){
      color_arg <- active_color
    }
    actionButton("click_in", label=label, style=color_arg)
  })
}


#' Server-side utility for \code{ibiVizEdit} that dynamically updates pre-processing PPG plot
#'
#' @export

basic_ppg <- function(ppg_data=NULL, brush_in=NULL){
  if(is.null(ppg_data)){
    p <- ppg_data_check_empty_plot()
  }
  else{
    p <- generate_ppg_data_check_plot(ppg_data=ppg_data)
    if(!is.null(brush_in)){
      p <- p + coord_cartesian(xlim=c(brush_in$xmin, brush_in$xmax))
    }
  }
  return(p)
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
