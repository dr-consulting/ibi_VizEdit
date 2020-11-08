#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny

app_server <- function( input, output, session ) {
  # --------------------------------------------------------------------------------------------------------------------
  # Server Setup
  # --------------------------------------------------------------------------------------------------------------------
  # Setting Session Timestamp:
  META_DATA[["sys_start_time"]] <- Sys.time()
  
  # Aquiring correct directory for user folder - intended to be invariant to system
  FILE_SETTINGS[["user_dir"]] <- get_user_folder(input)
  FILE_SETTINGS[["max_file"]] <- set_file_size_max()
  
  # Set working directory and make available for other utilities
  get_working_directory(input, "wd")
  output$wd_out <- generate_path_messages(default_text="Please select a working directory",
                                          msg_part1="Working Directory:", obj_name="wd")
  
  # Set filepaths for raw data and make available for other utilities
  store_raw_data_filepath(input, "ppg_file")
  output$ppg_file <- generate_path_messages(default_text="Please select a PPG data file",
                                            msg_part1="PPG File:", obj_name="ppg_file")
  
  store_raw_data_filepath(input, "timing_file")
  output$timing_file <- generate_path_messages(default_text="Please select a timing file (optional)",
                                               msg_part1="Timing File:", obj_name="timing_file")
  
  # --------------------------------------------------------------------------------------------------------------------
  # Button status reactivity
  # --------------------------------------------------------------------------------------------------------------------
  # Track text-field entry
  track_data_text_entry(input)
  turn_on_load_button()
  
  # --------------------------------------------------------------------------------------------------------------------
  # Data Entry Tab
  # --------------------------------------------------------------------------------------------------------------------
  
  # Creating Dynamic buttons for the data entry tab action buttons:
  callModule(dynamicClrButtonMod, "load", status_name="load", label="Load Settings")
  callModule(dynamicClrButtonMod, "save_progress", status_name="save_progress", label="Save Progress")
  callModule(dynamicClrButtonMod, "save_output", status_name="save_output", label="Save Outputs")
  
  # Load files - Should only work if correct settings identified in advance
  callModule(eventTriggerMod, "load", input_id="click_in", trigger_items=reactive({BUTTON_STATUS[["load"]]}),
             trigger_values=TRUE, trigger_object=TRIGGERS, trigger_id="load")
  
  observeEvent(TRIGGERS[["load"]], {
    if(TRIGGERS[["load"]] == TRUE){
      
      showModal(modalDialog(
        title = 'Loading Files',
        'Your files are currently being loaded. Large files may take longer.',
        size = 'm'
      ))
      
      load_files_and_settings(input)
      
      removeModal()
      
      if(is.null(STATIC_DATA[["ppg100"]]) & !is.null(STATIC_DATA[["orig_ppg"]])){
        STATIC_DATA[["ppg100"]] <- downsample_ppg_data(STATIC_DATA[["orig_ppg"]], STATIC_DATA[["hz_input"]])
      }
      
      if(!is.null(STATIC_DATA[["orig_ppg"]])){
        BUTTON_STATUS[["process_ppg"]] <- TRUE
      }
    }
  })
  
  # Save progress - will need to complete and test after all other parts integrated
  #   - Key addition is going to be to save partially completed file into an importable .RData object
  
  # Save outputs - also needs to be completed when rest of the GUI structure is built out
  
  
  # --------------------------------------------------------------------------------------------------------------------
  # Processing Tab
  # --------------------------------------------------------------------------------------------------------------------
  # Server-Side of PPG Processing Tab:
  callModule(dynamicClrButtonMod, "process_ppg", status_name="process_ppg", label="Process PPG")
  output$pre_process_ppg <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["ppg100"]], brush_in=input$pre_process_x)
  })
  
  output$pre_process_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["ppg100"]])
  })
  
  output$task_times <- renderTable({
    STATIC_DATA[["display_task_times"]]
  })
  
  output$peak_detect_tab <- renderTable({
    STATIC_DATA[["peak_detect_tab"]]
  })
  
  # Processing the data - setting up reactivity based on this button click
  callModule(eventTriggerMod, "process_ppg", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["process_ppg"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="process_ppg")
  
  observeEvent(TRIGGERS[["process_ppg"]], {
    if(TRIGGERS[["process_ppg"]] == TRUE){
      # Downsample the data
      STATIC_DATA[["processed_ppg"]] <- downsample_ppg_data(ppg_data=STATIC_DATA[["orig_ppg"]],
                                                            sampling_rate=STATIC_DATA[["hz_input"]],
                                                            downsampled_rate=STATIC_DATA[["hz_output"]])
      
      # Trim file to focus on the target observation period
      STATIC_DATA[["processed_ppg"]] <- trim_ppg_window(ppg_data=STATIC_DATA[["processed_ppg"]],
                                                        timing_data=STATIC_DATA[["display_task_times"]])
      
      # Perform basic filtering and pre-processing of the signal
      STATIC_DATA[["processed_ppg"]] <- filter_ppg(ppg_data=STATIC_DATA[["processed_ppg"]],
                                                   sampling_rate=STATIC_DATA[["hz_output"]])
      STATIC_DATA[["processed_ppg"]][["pnt_type"]] <- "original"
      
      
      # Save downsampled version of ppg signal for the "scroll" plot
      STATIC_DATA[["processed_ppg100"]] <- downsample_ppg_data(ppg_data=STATIC_DATA[["processed_ppg"]],
                                                               sampling_rate=STATIC_DATA[["hz_output"]])
      
      # Grab the list output from findpeaks
      ibi_output_list <- find_ibis(ppg_signal=STATIC_DATA[["processed_ppg"]][["PPG"]],
                                   sampling_rate=STATIC_DATA[["hz_output"]],
                                   min_time=min(STATIC_DATA[["display_task_times"]][["Start"]]),
                                   peak_iter=STATIC_DATA[["peak_iter"]])
      
      # Save the original set of IBIs detetectd at the start of the program run to a static data location
      STATIC_DATA[["orig_ibi"]] <- ibi_output_list[["IBI_out"]]
      STATIC_DATA[["orig_ibi"]][["pnt_type"]] <- "original"
      
      # Get the top row to display in the processing tab
      STATIC_DATA[["peak_detect_tab"]] <- head(ibi_output_list[["detection_settings"]], 3)
      
      # Generate the editable ppg data set
      if(is.null(DYNAMIC_DATA[["edited_ppg"]])){
        DYNAMIC_DATA[["edited_ppg"]] <- STATIC_DATA[["processed_ppg"]]
      }
      
      # Generate the editable ibi data set
      if(is.null(DYNAMIC_DATA[["edited_ibi"]])){
        DYNAMIC_DATA[["edited_ibi"]] <- STATIC_DATA[["orig_ibi"]]
      }
      
      # "Turn on" IBI buttons
      BUTTON_STATUS[["set_ibi_y_axis"]] <- TRUE
      BUTTON_STATUS[["show_ppg"]] <- TRUE
      BUTTON_STATUS[["ibi_drag_select"]] <- TRUE
      
      # "Turn on" PPG buttons
      BUTTON_STATUS[["set_ppg_y_axis"]] <- TRUE
      BUTTON_STATUS[["ppg_imp_mode"]] <- TRUE
    }
  })
  
  
  # --------------------------------------------------------------------------------------------------------------------
  # IBI Editing Tab
  # --------------------------------------------------------------------------------------------------------------------
  output$heads_up <- renderPrint({
    generate_heads_up_info(input, hover_id="hover_ibi", ibi_data=DYNAMIC_DATA[["edited_ibi"]])
  })
  callModule(dynamicClrButtonMod, "set_ibi_y_axis", status_name="set_ibi_y_axis", label="Set y-axis")
  callModule(dynamicClrButtonMod, "show_ppg", status_name="show_ppg", label="Show PPG", updated_label="Remove PPG",
             default_display_name="show_ppg_default")
  callModule(dynamicClrButtonMod, "ibi_drag_select", status_name="ibi_drag_select", label="Select Mode")
  callModule(dynamicClrButtonMod, "ibi_click_select", status_name="ibi_click_select", label="Click Mode")
  callModule(dynamicClrButtonMod, "average", status_name="average", label="Average")
  callModule(dynamicClrButtonMod, "combine", status_name="combine", label="Combine")
  callModule(dynamicClrButtonMod, "divide", status_name="divide", label="Divide")
  
  # Enable reactivity with the set_ibi_y_axis button
  callModule(eventTriggerMod, "set_ibi_y_axis", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["set_ibi_y_axis"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="set_ibi_y_axis")
  
  observeEvent(TRIGGERS[["set_ibi_y_axis"]], {
    if(TRIGGERS[["set_ibi_y_axis"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ibi"]])){
      TEMP_GRAPHICS_SETTINGS[["ymin"]] <- input$ibi_y_axis[1]
      TEMP_GRAPHICS_SETTINGS[["ymax"]] <- input$ibi_y_axis[2]
    }
  })
  
  # Enable reactivity using the show_ppg button
  callModule(eventTriggerMod, "show_ppg", input_id="click_in", trigger_items=reactive({BUTTON_STATUS[["show_ppg"]]}),
             trigger_values=TRUE, trigger_object=TRIGGERS, trigger_id="show_ppg")
  
  observeEvent(TRIGGERS[["show_ppg"]], {
    if(TRIGGERS[["show_ppg"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ibi"]]) &
       !is.null(DYNAMIC_DATA[["edited_ppg"]])){
      BUTTON_STATUS[["show_ppg_default"]] <- ifelse(BUTTON_STATUS[["show_ppg_default"]], FALSE, TRUE)
      TEMP_GRAPHICS_SETTINGS[["show_ppg"]] <- ifelse(TEMP_GRAPHICS_SETTINGS[["show_ppg"]], FALSE, TRUE)
    }
  })
  
  # Enable reactivity using the ibi_drag_select button
  callModule(eventTriggerMod, "ibi_drag_select", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["ibi_click_select"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="ibi_drag_select")
  
  observeEvent(TRIGGERS[["ibi_drag_select"]], {
    if(TRIGGERS[["ibi_drag_select"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ibi"]])){
      TEMP_GRAPHICS_SETTINGS[["select_mode"]] <- "drag"
      BUTTON_STATUS[["ibi_click_select"]] <- FALSE
      BUTTON_STATUS[["ibi_drag_select"]] <- ifelse(!BUTTON_STATUS[["ibi_drag_select"]], TRUE, FALSE)
    }
  })
  
  # Enable reactivity using the ibi_click_select_button
  callModule(eventTriggerMod, "ibi_click_select", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["ibi_drag_select"]]}), trigger_values=TRUE,
             trigger_object=TRIGGERS, trigger_id="ibi_click_select")
  
  observeEvent(TRIGGERS[["ibi_click_select"]], {
    if(TRIGGERS[["ibi_click_select"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ibi"]])){
      TEMP_GRAPHICS_SETTINGS[["select_mode"]] <- "click"
      BUTTON_STATUS[["ibi_drag_select"]] <- FALSE
      BUTTON_STATUS[["ibi_click_select"]] <- ifelse(!BUTTON_STATUS[["ibi_click_select"]], TRUE, FALSE)
    }
  })
  
  # Next building out capacity to store "selected_points" - using drag to select
  drag_point_collection(input, brush_id="drag_ibis")
  
  # Using click to select
  click_point_selection(input, click_id="click_ibis", dbl_click_id="clear_ibis")
  
  # Simple tracking for when each editing function can be used
  track_editing_options()
  
  # Obtain hover_point information
  hover_point_selection(input, hover_id="hover_ibi")
  
  # enable reactivity of average button
  callModule(eventTriggerMod, "average", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["average"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="average")
  
  observeEvent(TRIGGERS[["average"]], {
    if(TRIGGERS[["average"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ibi"]])){
      average_button_action(ibi_data=DYNAMIC_DATA[["edited_ibi"]], selected_points=DYNAMIC_DATA[["selected_points"]],
                            status=BUTTON_STATUS[["average"]])
    }
  })
  
  # enable reactivity of combine button
  callModule(eventTriggerMod, "combine", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["combine"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="combine")
  
  observeEvent(TRIGGERS[["combine"]], {
    if(TRIGGERS[["combine"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ibi"]])){
      combine_button_action(ibi_data=DYNAMIC_DATA[["edited_ibi"]],
                            selected_points=DYNAMIC_DATA[["selected_points"]],
                            status=BUTTON_STATUS[["combine"]])
    }
  })
  
  # enable reactivity of divide button
  callModule(eventTriggerMod, "divide", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["divide"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="divide")
  
  observeEvent(TRIGGERS[["divide"]], {
    if(TRIGGERS[["divide"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ibi"]])){
      divide_button_action(ibi_data=DYNAMIC_DATA[["edited_ibi"]], denom=input[["denom"]],
                           selected_points=DYNAMIC_DATA[["selected_points"]], status=BUTTON_STATUS[["divide"]])
    }
  })
  
  # Enable action by the uneditable button
  uneditable_button_action(input, ibi_data=DYNAMIC_DATA[["edited_ibi"]],
                           selected_points=DYNAMIC_DATA[["selected_points"]])
  
  restore_button_action(input, restore_id="undo_ibi", edited_data=DYNAMIC_DATA[["edited_ibi"]],
                        original_data=STATIC_DATA[["orig_ibi"]], brush_id="drag_ibis", ibi_or_ppg="ibi")
  
  save_screenshot(input, data=DYNAMIC_DATA[["edited_ibi"]], time_brush="editing_scroll_x", button_id="snapshot_ibi",
                  ibi_or_ppg="ibi")
  
  output$ibi_main_plot <- renderPlot({
    ibi_editing_plot(brush_in=input[["editing_scroll_x"]])
  })
  
  output$ibi_main_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["processed_ppg100"]])
  })
  
  # --------------------------------------------------------------------------------------------------------------------
  # PPG Editing Tab
  # --------------------------------------------------------------------------------------------------------------------
  callModule(dynamicClrButtonMod, "set_ppg_y_axis", status_name="set_ppg_y_axis", label="Set y-axis")
  callModule(dynamicClrButtonMod, "ppg_edit_mode", status_name="ppg_edit_mode", label="Insert/Remove")
  callModule(dynamicClrButtonMod, "ppg_imp_mode", status_name="ppg_imp_mode", label="Imputation Mode")
  callModule(dynamicClrButtonMod, "insert", status_name="insert", label="Insert")
  callModule(dynamicClrButtonMod, "remove", status_name="remove", label="Remove")
  callModule(dynamicClrButtonMod, "erase_ppg", status_name="erase_ppg", label="Erase PPG")
  callModule(dynamicClrButtonMod, "set_impute_window", status_name="set_impute_window", label="Lock Window")
  callModule(dynamicClrButtonMod, "set_valid_ibis", status_name="set_valid_ibis", label="Lock IBIs")
  callModule(dynamicClrButtonMod, "gp_impute", status_name="gp_impute",label="Run Bayesian GPM")
  
  # Enable reactivity with the set_ppg_y_axis button
  callModule(eventTriggerMod, "set_ppg_y_axis", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["set_ppg_y_axis"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="set_ppg_y_axis")
  
  observeEvent(TRIGGERS[["set_ppg_y_axis"]], {
    if(TRIGGERS[["set_ppg_y_axis"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ppg"]])){
      TEMP_GRAPHICS_SETTINGS[["ymin"]] <- input$ppg_y_axis[1]
      TEMP_GRAPHICS_SETTINGS[["ymax"]] <- input$ppg_y_axis[2]
    }
  })
  
  # Enable reactivity using the ppg_edit_mode button
  callModule(eventTriggerMod, "ppg_edit_mode", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["ppg_imp_mode"]]}), trigger_values=TRUE, trigger_object=TRIGGERS,
             trigger_id="ppg_edit_mode")
  
  observeEvent(TRIGGERS[["ppg_edit_mode"]], {
    if(TRIGGERS[["ppg_edit_mode"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ppg"]])){
      TEMP_GRAPHICS_SETTINGS[["ppg_mode"]] <- "edit"
      BUTTON_STATUS[["ppg_imp_mode"]] <- FALSE
      BUTTON_STATUS[["ppg_edit_mode"]] <- ifelse(!BUTTON_STATUS[["ppg_edit_mode"]], TRUE, FALSE)
    }
  })
  
  # Enable reactivity using the ppg_imp_mode button
  callModule(eventTriggerMod, "ppg_imp_mode", input_id="click_in",
             trigger_items=reactive({BUTTON_STATUS[["ppg_edit_mode"]]}), trigger_values=TRUE,
             trigger_object=TRIGGERS, trigger_id="ppg_imp_mode")
  
  observeEvent(TRIGGERS[["ppg_imp_mode"]], {
    if(TRIGGERS[["ppg_imp_mode"]] == TRUE & !is.null(DYNAMIC_DATA[["edited_ppg"]])){
      TEMP_GRAPHICS_SETTINGS[["select_mode"]] <- "impute"
      BUTTON_STATUS[["ppg_edit_mode"]] <- FALSE
      BUTTON_STATUS[["ppg_imp_mode"]] <- ifelse(!BUTTON_STATUS[["ppg_imp_mode"]], TRUE, FALSE)
    }
  })
  
  click_ppg_editing(input, click_id="add_ibi_ppg", dbl_click_id="del_ibi_ppg")
  
  output$ppg_main_plot <- renderPlot({
    ppg_editing_plot(brush_in=input$editing_scroll_x)
  })
  
  output$ppg_main_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["processed_ppg100"]])
  })
  
} # end of server
