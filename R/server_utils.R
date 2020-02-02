# Will need to have functionality to auto populate fields when existing data are loaded
# Probably want a modal dialog box that says something like "Import Settings"

#' Sever-side utility for \code{ibiVizEdit} that observes and updates directory information
#'
#' @export

get_working_directory <- function(input, input_name=NULL){
  observeEvent(input[[input_name]], {
    root_opts <- c(User=FILE_SETTINGS[["user_dir"]])
    dir_placeholder <- parseDirPath(roots=root_opts, input[[input_name]])

    if(length(dir_placeholder) != 0){
      FILE_SETTINGS[[input_name]] <- dir_placeholder
      root_opts <- c(wd=dir_placeholder)
    }

    shinyFileChoose(input, "ppg_file", roots=root_opts)
    shinyFileChoose(input, "timing_file", roots=root_opts)
  })
}

#' Server-side utility for \code{ibiVizEdit} that observes and updates raw data paths file paths
#'
#' @export

store_raw_data_filepath <- function(input, input_name=NULL){
  observeEvent(input[[input_name]], {
    if(!is.null(FILE_SETTINGS[["wd"]])){
      tmp_filepath_obj <- parseFilePaths(roots=c(wd=FILE_SETTINGS[["wd"]], User=FILE_SETTINGS[["user_dir"]]),
                                         input[[input_name]])
      if(nrow(tmp_filepath_obj) > 0){
        FILE_SETTINGS[[input_name]] <- as.character(tmp_filepath_obj$datapath)
      }
    }
  })
}


#' Server-side utility for \code{ibiVizEdit} that outputs relevant file and directory information when submitted
#'
#' @export

generate_path_messages <- function(default_text=NULL, msg_part1=NULL, obj_name=NULL){
  renderText({
    browser()
    msg_object <- FILE_SETTINGS[[obj_name]]
    txt <- default_text
    if(!is.null(msg_object)){
      txt <- paste(msg_part1, msg_object)
    }
    txt
  })
}

#' Server-side utility for \code{ibiVizEdit} that turns on "load" button
#'
#' @export

turn_on_load_button <- function(){
  BUTTON_STATUS[["load"]] <- observe({
    ifelse(!is.null(FILE_SETTINGS[["wd"]]) & !is.null(FILE_SETTINGS[["ppg_file"]]) & !is.null(META_DATA[["sub_id"]]),
           1, 0)
  })
}

#' Server-side utility for \code(ibiVizEdit) that monitors data entry values and updates them accordingly
#'
#' @export

processing_settings_observer <- function(input_name){
  PROCESSING_SETTINGS[[input_name]] <- observe({
    PROCESSING_SETTINGS[[input_name]] <- input[["input_name"]]
  })
}

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

dynamicClrButtonMod <- function(input, output, session, active=FALSE, label=NULL, hotkey=NULL, hotkey_map=NULL,
                                button_name="click_in", active_color=BUTTON_COLORS["standard"],
                                inactive_color=BUTTON_COLORS["inactive"]){

  output$rendered_button <- renderUI({
    #browser()
    color_arg <- inactive_color
    if(active){
      color_arg <- active_color
    }

    if(!is.null(hotkey) & !is.null(hotkey_map)){
      tags$script(HTML(track_hotkey_presses(key=hotkey, key_map=hotkey_map, button_name=button_name)))
    }

    actionButton(button_name, label=label, style=color_arg)
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
