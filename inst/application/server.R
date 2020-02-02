source("~/GitHub/IBI_VizEdit/R/server_utils.R")
source("~/GitHub/IBI_VizEdit/R/general_utils.R")

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
  output$ppg_file <-generate_path_messages(default_text="Please select a PPG data file",
                                           msg_part1="PPG File:", obj_name="ppg_file")

  store_raw_data_filepath(input, "timing_file")
  output$timing_file <-generate_path_messages(default_text="Please select a timing file (optional)",
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

  # Creating dynamic, updating values for numeric entry (enables easier load-from-file functionality)
  callModule(dynamicNumInputMod, "column_select", label="Column Index", value=PROCESSING_SETTINGS[["column_select"]])
  callModule(dynamicNumInputMod, "skip_rows", label="Header Lines:", value=PROCESSING_SETTINGS[["skip_rows"]])
  callModule(dynamicNumInputMod, "hz_input", label="Input Hz", value=PROCESSING_SETTINGS[["hz_input"]])
  callModule(dynamicNumInputMod, "hz_output", label="Output Hz:",  value=PROCESSING_SETTINGS[["hz_output"]])
  callModule(dynamicNumInputMod, "peak_iter", label="Processing Iterations:",
             value=PROCESSING_SETTINGS[["peak_iter"]])
  callModule(dynamicNumInputMod, "epoch_selected", label="Select Output Epochs:",
             value=PROCESSING_SETTINGS[["epoch_selected"]])
  callModule(dynamicSelectInputMod, "resp_age_grp", label="Select Age Group:",
             choices=PROCESSING_SETTINGS[["resp_age_grp_opts"]],
             choice_index=PROCESSING_SETTINGS[["resp_age_grp"]])
  callModule(dynamicCheckBoxInputMod, "epoch_outputs", label="Select Output Epochs",
             choices=PROCESSING_SETTINGS[["epoch_choices"]], selected=PROCESSING_SETTINGS[["epoch_selected"]])

  # Creating Dynamic buttons for the data entry tab action buttons:
  callModule(dynamicClrButtonMod, "load", status_name="load", label="Load Settings")
  callModule(dynamicClrButtonMod, "save_progress", status_name="save_progress", label="Save Progress")
  callModule(dynamicClrButtonMod, "save_output", status_name="save_output", label="Save Outputs")

  # --------------------------------------------------------------------------------------------------------------------
  # Processing Tab
  # --------------------------------------------------------------------------------------------------------------------
  # Server-Side of PPG Processing Tab:
  callModule(dynamicClrButtonMod, "process_ppg", active=as.logical(BUTTON_STATUS[["process_ppg"]]), label="Process PPG")
  output$pre_process_ppg <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["orig_ppg"]], brush_in=input$pre_process_x)
  })

  output$pre_process_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["ppg100"]])
  })

  output$task_times <- renderTable({
    STATIC_DATA[["task_times"]]
  })

  output$peak_detect_tab <- renderTable({
    STATIC_DATA[["peak_detect_tab"]]
  })

  # --------------------------------------------------------------------------------------------------------------------
  # IBI Editing Tab
  # --------------------------------------------------------------------------------------------------------------------
  callModule(headsUpInfo, "heads_up")
  callModule(dynamicClrButtonMod, "ibi_y_axis", active=as.logical(BUTTON_STATUS[["set_ibi_y_axis"]]),
             label="Set y-axis")
  callModule(dynamicClrTxtButtonMod, "show_ppg", active=as.logical(BUTTON_STATUS[["show_ppg"]]),
             default_display=BUTTON_STATUS[["show_ppg_default"]], default_text="Show PPG", updated_text="Remove PPG")
  callModule(dynamicClrButtonMod, "ibi_drag_select", active=as.logical(BUTTON_STATUS[["ibi_drag_select"]]),
             label="Select Mode")
  callModule(dynamicClrButtonMod, "ibi_click_select", active=as.logical(BUTTON_STATUS[["ibi_click_select"]]),
             label="Click Mode")
  callModule(dynamicClrButtonMod, "average", active=as.logical(BUTTON_STATUS[["average"]]), label="Average", hotkey="a",
             hotkey_map=EDIT_BUTTON_CLICKS)
  callModule(dynamicClrButtonMod, "combine", active=as.logical(BUTTON_STATUS[["combine"]]), label="Combine", hotkey="c",
             hotkey_map=EDIT_BUTTON_CLICKS)
  callModule(dynamicClrButtonMod, "divide", active=as.logical(BUTTON_STATUS[["divide"]]), label="Divide", hotkey="d",
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
  callModule(dynamicClrButtonMod, "ppg_y_axis", active=as.logical(BUTTON_STATUS[["set_ppg_y_axis"]]),
             label="Set y-axis")
  callModule(dynamicClrButtonMod, "ppg_edit_mode", active=as.logical(BUTTON_STATUS[["ppg_edit_mode"]]),
             label="Insert/Remove")
  callModule(dynamicClrButtonMod, "ppg_imp_mode", active=as.logical(BUTTON_STATUS[["ppg_imp_mode"]]),
             label="Imputation Mode")
  callModule(dynamicClrButtonMod, "insert", active=as.logical(BUTTON_STATUS[["insert"]]),
             label="Insert")
  callModule(dynamicClrButtonMod, "remove", active=as.logical(BUTTON_STATUS[["remove"]]),
             label="Remove")
  callModule(dynamicClrButtonMod, "erase_ppg", active=as.logical(BUTTON_STATUS[["erase_ppg"]]),
             label="Erase PPG")
  callModule(dynamicClrButtonMod, "set_impute_window", active=as.logical(BUTTON_STATUS[["set_impute_window"]]),
             label="Lock Window")
  callModule(dynamicClrButtonMod, "set_valid_ibis", active=as.logical(BUTTON_STATUS[["set_valid_ibis"]]),
             label="Lock IBIs")
  callModule(dynamicClrButtonMod, "gp_impute", active=as.logical(BUTTON_STATUS[["gp_impute"]]),
             label="Run Bayesian GPM")

  output$ppg_main_plot <- renderPlot({
    ppg_editing_plot(brush_in=input$editing_scroll_x)
  })

  output$ppg_main_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["ppg100"]])
  })

}
