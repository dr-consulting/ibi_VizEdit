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

#' Server-side utility for \code{ibiVizEdit} that dynamically switches actionButton UIs and Text Labels
#'
#' @export

dynamicClrTxtButtonMod <- function(input, output, session, active=FALSE, default_display=TRUE, default_text=NULL,
                                   updated_text=NULL, inactive_color=BUTTON_COLORS["inactive"],
                                   default_color=BUTTON_COLORS["standard"], updated_color=BUTTON_COLORS["warning"]){
  output$rendered_button <- renderUI({
    color_arg <- inactive_color
    label_arg <- default_text
    if(active){
      color_arg <- default_color
      if(!default_display){
        color_arg <- updated_color
        label_arg <- updated_text
      }
    }
    actionButton("click_in", label=label_arg, style=color_arg)
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


#' Server-side utility for \code{ibiVizEdit} that dynamically updates full IBI + PPG combo plots
#'
#' @export

ibi_editing_plot <- function(ibi_data=DYNAMIC_DATA[["edited_ibi"]], brush_in=NULL){
  if(is.null(ibi_data)){
    p <- ppg_data_check_empty_plot()
  }
  else{
    p <- generate_base_gui_plot(ibi_data=ibi_data, color_map=IBI_POINT_COLORS)
    if(!is.null(brush_in)){
      p <- p + coord_cartesian(xlim=c(brush_in$xmin, brush_in$xmax))
    }
    p <- add_task_v_lines(base_plot=p, timing_data=STATIC_DATA[["task_times"]])
    p <- add_ppg_waveform(base_plot=p, ppg_data=STATIC_DATA[["ppg100"]],
                          show_ppg=as.logical(BUTTON_STATUS[["show_ppg"]]))
    p <- highlight_ibis(base_plot=p, selected_points=DYNAMIC_DATA[["selected_points"]])
  }
  return(p)
}

#' Server-side utility for \code{ibiVizEdit} that updates summary stats displayed in text window
#'
#' @export

headsUpInfo <- function(input, output, session){

  temp_point <- reactive({
    req(DYNAMIC_DATA[["edited_IBI"]], SUMMARY_STATS[[c("mean_HR", "mean_R")]])
    nearPoints(DYNAMIC_DATA[["edited_IBI"]][c("IBI", "Time")], coordinfo=input$hover_ibi, maxpoints=1)
  })

  output$heads_up <- renderPrint({
    cat("Average HR & Resp:\n")
    cat("HR BPM:", "\t\t\t", round(SUMMARY_STATS[["mean_HR"]], 2))
    cat("Resp per min:", "\t", round(SUMMARY_STATS[["mean_R"]], 2))
    cat("Total IBIs:", "\t\t", nrow(DYNAMIC_DATA[["edited_ibi"]]))
    cat("Edited IBIs:", "\t", paste0(round(SUMMARY_STATS[["tot_edits"]]/nrow(DYNAMIC_DATA[["edited_ibi"]]), 2), "%"))
    cat("\nNear Point:\n")
    temp_point()
  })
}


#' Server-side utility for \code{ibiVizEdit} that defines main PPG plot for GUI editing
#'
#' @export

ppg_editing_plot <- function(ibi_data=DYNAMIC_DATA[["edited_ibi"]], brush_in=NULL){
  if(is.null(ibi_data)){
    p <- ppg_data_check_empty_plot()
  }
  else{
    p <- generate_base_gui_plot(ibi_data=ibi_data, color_map=IBI_POINT_COLORS)
    if(!is.null(brush_in)){
      p <- p + coord_cartesian(xlim=c(brush_in$xmin, brush_in$xmax))
    }
    p <- add_task_v_lines(base_plot=p, timing_data=STATIC_DATA[["task_times"]])
    p <- add_ppg_waveform(base_plot=p, ppg_data=DYNAMIC_DATA[["edited_ppg"]],
                          show_ppg=TRUE)
    p <- highlight_ibis(base_plot=p, selected_points=DYNAMIC_DATA[["selected_points"]])
  }
  return(p)
}
