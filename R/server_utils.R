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
    msg_object <- FILE_SETTINGS[[obj_name]]
    txt <- default_text
    if(!is.null(msg_object)){
      txt <- paste(msg_part1, msg_object)
    }
    txt
  })
}


#' Server-side utility for \code{ibiVizEdit} that tracks and updates text field data entry
#'
#' @export
track_data_text_entry <- function(input){
    observe({
      if(isTruthy(input[["sub_id"]])){
        META_DATA[["sub_id"]] <- input[["sub_id"]]
      }

      if(isTruthy(input[["secondary_id"]])){
        META_DATA[["secondary_id"]] <- input[["secondary_id"]]
      }

      if(isTruthy(input[["optional_id"]])){
        META_DATA[["optional_id"]] <- input[["optional_id"]]
      }

      if(isTruthy(input[["editor"]])){
        META_DATA[["editor"]] <- input[["editor"]]
      }
    })
}


#' Server-side utility for \code{ibiVizEdit} that turns on "load" button
#'
#' @export

turn_on_load_button <- function(){
  observe({
    BUTTON_STATUS[["load"]] <- ifelse(!is.null(FILE_SETTINGS[["wd"]]) & !is.null(FILE_SETTINGS[["ppg_file"]]) &
                                        !is.null(META_DATA[["sub_id"]]) & !is.null(META_DATA[["secondary_id"]]) &
                                        !is.null(META_DATA[["editor"]]), 1, 0)
  })
}


#' Server-side utility for \code{ibiVizEdit} that loads data using specifications set in Data Entry tab
#'
#' @export

load_files_and_settings <- function(input){
  if(BUTTON_STATUS[["load"]] == 1){
    STATIC_DATA[["column_select"]] <- input[["column_select"]]
    STATIC_DATA[["skip_rows"]] <- input[["skip_rows"]]
    STATIC_DATA[["hz_input"]] <- input[["hz_input"]]
    STATIC_DATA[["hz_output"]] <- input[["hz_output"]]
    STATIC_DATA[["resp_age_group"]] <- input[["resp_age_grp"]]
    STATIC_DATA[["peak_iter"]] <- input[["peak_iter"]]
    STATIC_DATA[["epoch_outputs"]] <- input[["epoch_outputs"]]
    STATIC_DATA[["case_id"]] <- paste(input[["sub_id"]], input[["secondary_id"]], sep="_")
    STATIC_DATA[["orig_ppg"]] <- load_ppg(FILE_SETTINGS[["ppg_file"]], skip_lines=STATIC_DATA[["skip_rows"]],
                                          column=STATIC_DATA[["column_select"]],
                                          sampling_rate=STATIC_DATA[["hz_input"]])

    STATIC_DATA[["task_times"]] <- load_timing_data(FILE_SETTINGS[["timing_file"]], case_id=STATIC_DATA[["case_id"]])

    STATIC_DATA[["display_task_times"]] <- create_gui_timing_table(STATIC_DATA[["task_times"]])
  }

  else{
    warning("You have not entered enough information to process the data yet")
  }
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
    p <- ibi_value_label(base_plot=p, hover_point=DYNAMIC_DATA[["hover_point"]])

    if(!is.null(brush_in)){
      if(!is.null(TEMP_GRAPHICS_SETTINGS[["ymin"]])){
        p <- p + coord_cartesian(xlim=c(brush_in$xmin, brush_in$xmax),
                                 ylim=c(TEMP_GRAPHICS_SETTINGS[["ymin"]], TEMP_GRAPHICS_SETTINGS[["ymax"]]))
      }

      else{
        p <- p + coord_cartesian(xlim=c(brush_in$xmin, brush_in$xmax))
      }
    }

    p <- add_task_v_lines(base_plot=p, timing_data=STATIC_DATA[["display_task_times"]])
    p <- add_ppg_waveform(base_plot=p, ppg_data=STATIC_DATA[["processed_ppg100"]],
                          show_ppg=as.logical(TEMP_GRAPHICS_SETTINGS[["show_ppg"]]))
    p <- highlight_ibis(base_plot=p, selected_points=DYNAMIC_DATA[["selected_points"]])
  }
  return(p)
}

#' Server-side utility for \code{ibiVizEdit} that updates summary stats displayed in text window
#'
#' @export

generate_heads_up_info <- function(input, hover_id=NULL, ibi_data=NULL){
  req(ibi_data)
  SUMMARY_STATS[["mean_HR"]] <- estimate_average_HR(ibi_data)
  cat("Average HR (BPM):\n")
  cat(round(SUMMARY_STATS[["mean_HR"]], 2))
  cat("\nTotal IBIs:\n")
  cat(nrow(ibi_data))
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


#' Server-side utility for \code{ibiVizEdit} that enables Event Observation from action buttons inside modules
#'
#' Note that currently the logic on whether a function (func) should run needs to be contained inside that function
#'
#' @export

eventTriggerMod <- function(input, output, session, input_id=NULL, trigger_items=NULL, trigger_values=NULL,
                            trigger_object=NULL, trigger_id=NULL){
  observeEvent(input[[input_id]], {
    trigger_object[[trigger_id]] <- FALSE

    if(trigger_items() == trigger_values){
      trigger_object[[trigger_id]] <- TRUE
    }
  })
}


#' Server side function that "collects" points for editing when in click and drag selection mode
#'
#' @export
#'

drag_point_collection <- function(input, brush_id, valid_status="drag",
                                  status_var=reactive({TEMP_GRAPHICS_SETTINGS[["select_mode"]]})){
  observeEvent(input[[brush_id]], {
    if(status_var() == valid_status){
      if(!is.null(input[[brush_id]])){
        DYNAMIC_DATA[["selected_points"]] <- brushedPoints(DYNAMIC_DATA[["edited_ibi"]], input[[brush_id]])
      }
      else{
        DYNAMIC_DATA[["selected_points"]] <- NULL
      }
    }
  },
  ignoreNULL = FALSE)


}

#' Server side function that "collects" points for editing when in click ibi selection mode
#'
#' @export
#'

click_point_selection <- function(input, click_id, dbl_click_id, valid_status="click",
                                  status_var=reactive({TEMP_GRAPHICS_SETTINGS[["select_mode"]]})){
  observeEvent(input[[click_id]], {
    if(status_var() == valid_status & !is.null(input[[click_id]])){
      if(is.null(DYNAMIC_DATA[["selected_points"]])){
        DYNAMIC_DATA[["selected_points"]] <- nearPoints(DYNAMIC_DATA[["edited_ibi"]], input[[click_id]], xvar="Time",
                                                        yvar="IBI", maxpoints = 1)
      }

      else{
        tmp_clicked <- nearPoints(DYNAMIC_DATA[["edited_ibi"]], input[[click_id]], xvar="Time",
                                  yvar="IBI", maxpoints = 1)

        tmp_clicked <- rbind(tmp_clicked, DYNAMIC_DATA[["selected_points"]])

        min_clicked_time <- min(tmp_clicked[["Time"]])
        max_clicked_time <- max(tmp_clicked[["Time"]])

        tmp_clicked <- DYNAMIC_DATA[["edited_ibi"]][between(DYNAMIC_DATA[["edited_ibi"]][["Time"]],
                                                            min_clicked_time, max_clicked_time), ]

        DYNAMIC_DATA[["selected_points"]] <- tmp_clicked
      }
    }
  })

  observeEvent(input[[dbl_click_id]], {
    if(status_var() == valid_status){
      if(!is.null(DYNAMIC_DATA[["selected_points"]])){
        DYNAMIC_DATA[["selected_points"]] <- NULL
      }
    }
  })
}


#' Server side function that tracks and updates the status of the editing functions
#'
#' @export
#'

track_editing_options <- function(){
  observeEvent(DYNAMIC_DATA[["selected_points"]], {
    if(!is.null(DYNAMIC_DATA[["selected_points"]])){

      if(nrow(DYNAMIC_DATA[["selected_points"]]) == 1){
        BUTTON_STATUS[["divide"]] <- TRUE
      }

      else if(nrow(DYNAMIC_DATA[["selected_points"]]) > 1){
        BUTTON_STATUS[["average"]] <- TRUE
        BUTTON_STATUS[["combine"]] <- TRUE
      }
    }

    else{
      BUTTON_STATUS[["divide"]] <- FALSE
      BUTTON_STATUS[["average"]] <- FALSE
      BUTTON_STATUS[["combine"]] <- FALSE
    }
  }, ignoreNULL = FALSE)
}

#' Server side function to acquire hover points
#'
#' @export
#'

hover_point_selection <- function(input, hover_id, ibi_data=DYNAMIC_DATA[["edited_ibi"]]){
  observeEvent(input[[hover_id]], {

    if(!is.null(input[[hover_id]])){
      tmp_point <- nearPoints(ibi_data, coordinfo = input[[hover_id]], maxpoints = 1)

      if(nrow(tmp_point) == 1){
        DYNAMIC_DATA[["hover_point"]] <- tmp_point
      }

      else{
        DYNAMIC_DATA[["hover_point"]] <- NULL
      }
    }
  }, ignoreNULL = FALSE)
}

#' Server side function to facilitate combining multiple points
#'
#' @export
#'

#' Server side function to factiliate averaging mulitple points
#'
#' @export
#'

#' Server side function to facilitate division of a single point
