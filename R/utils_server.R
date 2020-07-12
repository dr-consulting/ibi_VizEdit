#' Sever-side utility for \code{ibiVizEdit} that observes and updates directory information
#'
#' @param input,input_name {shiny} and {ibiVizEdit} internals for setting working directory
#'
#' @importFrom shinyFiles parseDirPath shinyFileChoose

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
#' @param input,input_name {shiny} and {ibiVizEdit} internals for defining raw data path
#' 
#' @importFrom shinyFiles parseFilePaths

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
#' @param default_text the default message to display
#' @param msg_part1 the first portion of the message which remains static. 
#' @param obj_name the object name referred to at the end of the displayed message. Typically a file or directory path
#' 
#' @return a message to be displayed in the appropriate location in the {ibiVizEdit} ui

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
#' @param input {shiny} internal

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

turn_on_load_button <- function(){
  observe({
    BUTTON_STATUS[["load"]] <- ifelse(!is.null(FILE_SETTINGS[["wd"]]) & !is.null(FILE_SETTINGS[["ppg_file"]]) &
                                        !is.null(META_DATA[["sub_id"]]) & !is.null(META_DATA[["secondary_id"]]) &
                                        !is.null(META_DATA[["editor"]]), 1, 0)
  })
}


#' Server-side utility for \code{ibiVizEdit} that loads data using specifications set in Data Entry tab
#' @param input {shiny} internal

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
    STATIC_DATA[["optional_id"]] <- input[["optional_id"]]
    STATIC_DATA[["orig_ppg"]] <- load_ppg(FILE_SETTINGS[["ppg_file"]], skip_lines=STATIC_DATA[["skip_rows"]],
                                          column=STATIC_DATA[["column_select"]],
                                          sampling_rate=STATIC_DATA[["hz_input"]])
    STATIC_DATA[["task_times"]] <- load_timing_data(FILE_SETTINGS[["timing_file"]], case_id=STATIC_DATA[["case_id"]])
    STATIC_DATA[["display_task_times"]] <- create_gui_timing_table(STATIC_DATA[["task_times"]])
    FILE_SETTINGS[["out_dir"]] <- create_and_return_output_dir(FILE_SETTINGS[["wd"]], STATIC_DATA[["case_id"]],
                                                               optional_id=META_DATA[["optional_id"]])
    FILE_SETTINGS[["screenshot_out_dir"]] <- create_and_return_screenshot_dir(FILE_SETTINGS[["out_dir"]])
  }

  else{
    warning("You have not entered enough information to process the data yet")
  }
}


#' Server-side utility for \code{ibiVizEdit} that dynamically updates pre-processing PPG plot
#'
#' @param ppg_data PPG data series
#' @param brush_in the brush used to define the time window for inspection
#' @importFrom ggplot2 coord_cartesian

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
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session
#' @param brush_in the brush used to define the time window for inspection
#' 
#' @importFrom ggplot2 coord_cartesian

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
#' @param input {shiny} internal
#' @param hover_id internal name for the hover input object
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session 

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
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session
#' @param brush_in the brush used to define the time window for inspection
#' 
#' @importFrom ggplot coord_cartesian

ppg_editing_plot <- function(ibi_data=DYNAMIC_DATA[["edited_ibi"]], brush_in=NULL){
  if(is.null(ibi_data)){
    p <- ppg_data_check_empty_plot()
  }
  else{
    p <- generate_base_gui_plot(ibi_data=ibi_data, color_map=IBI_POINT_COLORS)
    if(!is.null(brush_in)){
      p <- p + coord_cartesian(xlim=c(brush_in$xmin, brush_in$xmax))
    }
    p <- add_task_v_lines(base_plot=p, timing_data=STATIC_DATA[["display_task_times"]])
    p <- add_ppg_waveform(base_plot=p, ppg_data=DYNAMIC_DATA[["edited_ppg"]],
                          show_ppg=TRUE)
    p <- highlight_ibis(base_plot=p, selected_points=DYNAMIC_DATA[["selected_points"]])
  }
  return(p)
}


#' Server side function that "collects" points for editing when in click and drag selection mode
#' 
#' @param input {shiny} internal
#' @param brush_id the brush id used to perform point selection in the "main" editing plot on a given panel
#' @param valid_status defaults to "drag" - the other option is "click" in terms of point selection
#' @param status_var the reactiveValues that "track" whether the select_mode status is "drag" or "click"

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
#' @param input {shiny} internal
#' @param click_id the click id used to perform point selection in the "main" editing plot on a given panel
#' @param dbl_click_id the double click id used to perform point selection in the "main" editing plot on a given panel. 
#' This action is used to reset or "de-select" any points that were previously highlighted by the user. 
#' @param valid_status defaults to "click" - the other option is "drag" in terms of point selection
#' @param status_var the reactiveValues that "track" whether the select_mode status is "drag" or "click"
#' 
#' @importFrom dplyr between

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
track_editing_options <- function(){
  observeEvent(DYNAMIC_DATA[["selected_points"]], {
    if(!is.null(DYNAMIC_DATA[["selected_points"]])){

      if(nrow(DYNAMIC_DATA[["selected_points"]]) == 1){
        BUTTON_STATUS[["divide"]] <- TRUE
        BUTTON_STATUS[["average"]] <- FALSE
        BUTTON_STATUS[["combine"]] <- FALSE
      }

      if(nrow(DYNAMIC_DATA[["selected_points"]]) > 1){
        BUTTON_STATUS[["divide"]] <- FALSE
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


#' Server side function to extract necessary information from the IBI time series
#'
#' Takes in the ibi time series and extracts information used in the average, combine, and divide computations
#' 
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session
#' @param selected_points the points defined by using the "drag" or "click" selection method 

extract_ibi_editing_info <- function(ibi_data, selected_points=NULL){
  ibi_info <- list(
    first_ibi=ibi_data[["IBI"]][1],
    max_time_selected=max(selected_points[["Time"]]),
    orig_time_before=ibi_data[["Time"]][ibi_data[["Time"]] < min(selected_points[["Time"]])],
    orig_time_after=ibi_data[["Time"]][ibi_data[["Time"]] > max(selected_points[["Time"]])],
    max_orig_time_before=max(ibi_data[["Time"]][ibi_data[["Time"]] < min(selected_points[["Time"]])])
  )

  return(ibi_info)
}


#' Server side function to facilitate combining multiple points
#' 
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session
#' @param selected_points the points defined by using the "drag" or "click" selection method 
#' @param status the status of the combine button - whether it can be used or not

combine_button_action <- function(ibi_data, selected_points=NULL, status=NULL){
  if(status){
    info <- extract_ibi_editing_info(ibi_data, selected_points)
    info[["combined_ibi"]] <- sum(selected_points[["IBI"]])

    if(length(info[["orig_time_before"]]) == 0){
      # Effectively the same as deleting the first point and adding its IBI to the second point
      new_data <- ibi_data[-1,]
      new_data[["pnt_type"]][1] <- "combined"
      new_data[["IBI"]][1] <- info[["combined_ibi"]]
    }

    else{

      if(length(info[["orig_time_after"]]) == 0){
        time_new <- c(info[["orig_time_before"]], info[["max_orig_time_before"]] + info[["combined_ibi"]])
      }

      else{
        time_new <- c(info[["orig_time_before"]], info[["max_orig_time_before"]] + info[["combined_ibi"]],
                      info[["orig_time_after"]])
      }
      ibi_new <- time_diff(time_new)[-1]  # Dropping the first value to preserve original time stamps
      ibi_new <- c(info[["first_ibi"]], ibi_new)
      pnt_type <- ibi_data[["pnt_type"]][ibi_data[["Time"]] %in% time_new]
      pnt_type[time_new == info[["max_time_selected"]]] <- "combined"

      new_data <- data.frame(IBI=ibi_new,
                             Time=time_new,
                             pnt_type=pnt_type,
                             stringsAsFactors = FALSE)
    }

    DYNAMIC_DATA[["edited_ibi"]] <- new_data
    DYNAMIC_DATA[["selected_points"]] <- NULL
  }
}


#' Server side function to facilitate division of a single point
#'
#' Takes a single point and divides it into n points as determined by the user-specified denominator - defaults to 2 in
#' the UI
#' 
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session
#' @param denom the denominator specified by the user. Determines the number of points to divide the selected IBI value
#' evenly into.
#' @param selected_points the points defined by using the "drag" or "click" selection method 
#' @param status the status of the divide button - whether it can be used or not

divide_button_action <- function(ibi_data, denom=NULL, selected_points=NULL, status=NULL){
  if(status){
    info <- extract_ibi_editing_info(ibi_data, selected_points)
    info[["divided_ibis"]] <- rep(selected_points[["IBI"]]/denom, denom)

    if(length(info[["orig_time_before"]]) == 0){
      ibi_new <- c(info[["divided_ibis"]], ibi_data[["IBI"]][-1])
      time_new <- ibi_data[["Time"]]
      pnt_type <- c(rep("divided", length(info[["divided_ibis"]])), ibi_data[["pnt_type"]][-1])

      for(i in 1:(length(info[["divided_ibis"]]) - 1)){
        time_new <- c(time_new[1] - info[["divided_ibis"]][i], time_new)
      }
    }

    else if(length(info[["orig_time_after"]]) == 0){
      ibi_new <- c(ibi_data[["IBI"]][-nrow(ibi_data)], info[["divided_ibis"]])
      time_new <- ibi_data[["Time"]]

      pnt_type <- c(ibi_data[["pnt_type"]][-nrow(ibi_data)],
                    rep("divided", length(info[["divided_ibis"]])))

      for(i in 1:(length(info[["divided_ibis"]]) - 1)){
        time_new <- c(time_new, time_new[length(time_new)] + info[["divided_ibis"]][i])
      }
    }

    else {
      ibi_before  <- ibi_data[["IBI"]][ibi_data[["Time"]] < selected_points[["Time"]]]
      ibi_after <- ibi_data[["IBI"]][ibi_data[["Time"]] > selected_points[["Time"]]]

      pnt_type_before <- ibi_data[["pnt_type"]][ibi_data[["Time"]] < selected_points[["Time"]]]
      pnt_type_after <- ibi_data[["pnt_type"]][ibi_data[["Time"]] > selected_points[["Time"]]]
      time_before <- info[["orig_time_before"]]

      for(i in 1:(length(info[["divided_ibis"]]))){
        time_before <- c(time_before, time_before[length(time_before)] + info[["divided_ibis"]][i])
      }

      ibi_new <- c(ibi_before, info[["divided_ibis"]], ibi_after)
      time_new <- c(time_before, info[["orig_time_after"]])
      pnt_type <- c(pnt_type_before, rep('divided', length(info[["divided_ibis"]])), pnt_type_after)
    }

    new_data <- data.frame(IBI=ibi_new,
                           Time=time_new,
                           pnt_type=pnt_type,
                           stringsAsFactors = FALSE)
    DYNAMIC_DATA[["edited_ibi"]] <- new_data
    DYNAMIC_DATA[["selected_points"]] <- NULL
  }
}


#' Server side function to facilitate averaging multiple points
#'
#' Takes a single point and divides it into n points as determined by the user-specified denominator - defaults to 2 in
#' the UI
#' 
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session
#' @param selected_points the points defined by using the "drag" or "click" selection method 
#' @param status the status of the average button - whether it can be used or not

average_button_action <- function(ibi_data, selected_points=NULL, status=NULL){
  if(status){
    info <- extract_ibi_editing_info(ibi_data, selected_points)
    avg_value <- mean(selected_points[["IBI"]])
    info[["averaged_ibis"]] <- rep(mean(selected_points[["IBI"]]), nrow(selected_points))

    if(length(info[["orig_time_before"]]) == 0){
      ibi_new <- c(info[["averaged_ibis"]], ibi_data[["IBI"]][-1])
      time_new <- ibi_data[["Time"]]
      pnt_type <- c(rep("averaged", length(info[["averaged_ibis"]])), ibi_data[["pnt_type"]][-1])

      for(i in 1:(length(info[["averaged_ibis"]]) - 1)){
        time_new <- c(time_new[1] - info[["averaged_ibis"]][i], time_new)
      }
    }

    else if(length(info[["orig_time_after"]]) == 0){
      ibi_new <- c(ibi_data[["IBI"]][-nrow(ibi_data)], info[["averaged_ibis"]])
      time_new <- ibi_data[["Time"]]

      pnt_type <- c(ibi_data[["pnt_type"]][-nrow(ibi_data)],
                    rep("averaged", length(info[["averaged_ibis"]])))

      for(i in 1:(length(info[["averaged_ibis"]]) - 1)){
        time_new <- c(time_new, time_new[length(time_new)] + info[["averaged_ibis"]][i])
      }
    }

    else {
      ibi_before  <- ibi_data[["IBI"]][ibi_data[["Time"]] < min(selected_points[["Time"]])]
      ibi_after <- ibi_data[["IBI"]][ibi_data[["Time"]] > max(selected_points[["Time"]])]

      pnt_type_before <- ibi_data[["pnt_type"]][ibi_data[["Time"]] < min(selected_points[["Time"]])]
      pnt_type_after <- ibi_data[["pnt_type"]][ibi_data[["Time"]] > max(selected_points[["Time"]])]
      time_before <- info[["orig_time_before"]]

      for(i in 1:(length(info[["averaged_ibis"]]))){
        time_before <- c(time_before, time_before[length(time_before)] + info[["averaged_ibis"]][i])
      }

      ibi_new <- c(ibi_before, info[["averaged_ibis"]], ibi_after)
      time_new <- c(time_before, info[["orig_time_after"]])
      pnt_type <- c(pnt_type_before, rep('averaged', length(info[["averaged_ibis"]])), pnt_type_after)
    }

    new_data <- data.frame(IBI=ibi_new,
                           Time=time_new,
                           pnt_type=pnt_type,
                           stringsAsFactors = FALSE)
    DYNAMIC_DATA[["edited_ibi"]] <- new_data
    DYNAMIC_DATA[["selected_points"]] <- NULL
  }
}


#' Server side utility that marks selected points as uneditable
#' 
#' @param input {shiny} internal
#' @param ibi_data IBI data stored in a list of reactiveValues and edited during the user's {ibiVizEdit} session
#' @param selected_points the points defined by using the "drag" or "click" selection method 

uneditable_button_action <- function(input, ibi_data, selected_points=NULL){
  observeEvent(input[["uneditable"]], {
    if(!is.null(selected_points)){
      ibi_data[["pnt_type"]][ibi_data[["Time"]] %in% selected_points[["Time"]]] <- "uneditable"
      DYNAMIC_DATA[["edited_ibi"]] <- ibi_data
    }
  })
}

#' Server side utility that takes restores all IBIs within the selected window
#'
#' @param input {shiny} internal 
#' @param restore_id id value for the button used to enable the restore action
#' @param edited_data data stored in a list of reactiveValues that contains "in-progress" edits during the user session
#' @param original_data data stored in a list of reactiveValues that contains the "original" data as it existed 
#' following preliminary processing but before manual editing. 
#' @param brush_id id for the brush that defines the section of data to restore
#' @param ibi_or_ppg used to define whether the data being restored is from the IBI or PPG series
#' @importFrom dplyr between

restore_button_action <- function(input, restore_id, edited_data, original_data, brush_id, ibi_or_ppg=NULL){
  observeEvent(input[[restore_id]], {
    if(!is.null(input[[brush_id]])){
      time_min <- input[[brush_id]]$xmin
      time_max <- input[[brush_id]]$xmax
      select_vals <- between(edited_data[["Time"]], time_min, time_max)
      edited_data <- edited_data[!select_vals, ]
      edited_data <- rbind(edited_data, original_data[between(original_data[["Time"]], time_min, time_max), ])
      edited_data <- edited_data[order(edited_data[["Time"]], decreasing=FALSE), ]

      if(ibi_or_ppg == "ibi"){
        DYNAMIC_DATA[["edited_ibi"]] <- edited_data
      }

      if(ibi_or_ppg == "ppg"){
        DYNAMIC_DATA[["edited_ppg"]] <- edited_data
      }
    }
  })
}

