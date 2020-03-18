source("~/dr-consulting_GH/ibi_VizEdit/R/server_utils.R")
source("~/dr-consulting_GH/ibi_VizEdit/R/general_utils.R")

server <- function(input, output, session){
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
      load_files_and_settings(input)
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
    basic_ppg(ppg_data=STATIC_DATA[["orig_ppg"]], brush_in=input$pre_process_x)
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

  # --------------------------------------------------------------------------------------------------------------------
  # IBI Editing Tab
  # --------------------------------------------------------------------------------------------------------------------
  callModule(headsUpInfo, "heads_up")
  callModule(dynamicClrButtonMod, "ibi_y_axis", status_name="set_ibi_y_axis", label="Set y-axis")
  callModule(dynamicClrButtonMod, "show_ppg", status_name="show_ppg", label="Show PPG", updated_label="Remove PPG",
             default_display_name="show_ppg_default")
  callModule(dynamicClrButtonMod, "ibi_drag_select", status_name="ibi_drag_select", label="Select Mode")
  callModule(dynamicClrButtonMod, "ibi_click_select", status_name="ibi_click_select", label="Click Mode")
  callModule(dynamicClrButtonMod, "average", status_name="average", label="Average", hotkey="a",
             hotkey_map=EDIT_BUTTON_CLICKS)
  callModule(dynamicClrButtonMod, "combine", status_name="combine", label="Combine", hotkey="c",
             hotkey_map=EDIT_BUTTON_CLICKS)
  callModule(dynamicClrButtonMod, "divide", status_name="divide", label="Divide", hotkey="d",
             hotkey_map=EDIT_BUTTON_CLICKS)

  output$ibi_main_plot <- renderPlot({
    ibi_editing_plot(brush_in=input$editing_scroll_x)
  })

  output$ibi_main_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["ppg100"]])
  })

  # --------------------------------------------------------------------------------------------------------------------
  # PPG Editing Tab
  # --------------------------------------------------------------------------------------------------------------------
  callModule(dynamicClrButtonMod, "ppg_y_axis", status_name="set_ppg_y_axis", label="Set y-axis")
  callModule(dynamicClrButtonMod, "ppg_edit_mode", status_name="ppg_edit_mode", label="Insert/Remove")
  callModule(dynamicClrButtonMod, "ppg_imp_mode", status_name="ppg_imp_mode", label="Imputation Mode")
  callModule(dynamicClrButtonMod, "insert", status_name="insert", label="Insert")
  callModule(dynamicClrButtonMod, "remove", status_name="remove", label="Remove")
  callModule(dynamicClrButtonMod, "erase_ppg", status_name="erase_ppg", label="Erase PPG")
  callModule(dynamicClrButtonMod, "set_impute_window", status_name="set_impute_window", label="Lock Window")
  callModule(dynamicClrButtonMod, "set_valid_ibis", status_name="set_valid_ibis", label="Lock IBIs")
  callModule(dynamicClrButtonMod, "gp_impute", status_name="gp_impute",label="Run Bayesian GPM")

  output$ppg_main_plot <- renderPlot({
    ppg_editing_plot(brush_in=input$editing_scroll_x)
  })

  output$ppg_main_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["ppg100"]])
  })

}
