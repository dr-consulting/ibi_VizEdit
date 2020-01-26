source("~/GitHub/IBI_VizEdit/R/server_utils.R")
source("~/GitHub/IBI_VizEdit/R/general_utils.R")

server <- function(input, output, session){
  # --------------------------------------------------------------------------------------------------------------------
  # Server Setup
  # --------------------------------------------------------------------------------------------------------------------
  # Setting Session Timestamp:
  META_DATA[["sys_start_time"]] <- Sys.time()

  # Aquiring correct directory for user folder - intended to be invariant to system
  FILE_SETTINGS[["user_dir"]] <- get_user_folder()
  FILE_SETTINGS[["max_file"]] <- set_file_size_max()

  # --------------------------------------------------------------------------------------------------------------------
  # Data Entry Tab
  # --------------------------------------------------------------------------------------------------------------------
  # Creating dynamic, updating values for text entry (enables easier load-from-file functionality)
  callModule(dynamicTextInputMod, "sub_id", label="Subect ID:", value=META_DATA[["sub_id"]])
  callModule(dynamicTextInputMod, "secondary_id", label="Time/Task ID:", value=META_DATA[["secondary_id"]])
  callModule(dynamicTextInputMod, "optional_id", label="(Optional) Study ID:", value=META_DATA[["optional_id"]])
  callModule(dynamicTextInputMod, "editor", label="Editor Name:", value=META_DATA[["editor"]])

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
  callModule(dynamicClrButtonMod, "load", active=as.logical(BUTTON_STATUS[["load"]]), label="Load Settings")
  callModule(dynamicClrButtonMod, "save_progress", active=as.logical(BUTTON_STATUS[["save_progress"]]),
                                                                     label="Save Progress")
  callModule(dynamicClrButtonMod, "save_output", active=as.logical(BUTTON_STATUS[["save_output"]]),
                                                                   label="Save Outputs")

  # --------------------------------------------------------------------------------------------------------------------
  # Processing Tab
  # --------------------------------------------------------------------------------------------------------------------
  # Server-Side of PPG Processing Tab:
  callModule(dynamicClrButtonMod, "process_ppg", active=as.logical(BUTTON_STATUS[["process_ppg"]]), label="Process PPG")
  output$pre_process_ppg <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["orig_ppg"]], brush_in=input$pre_process_x)
  })

  output$pre_process_scroll <- renderPlot({
    basic_ppg(ppg_data=STATIC_DATA[["orig_ppg"]])
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
  callModule(dynamicClrButtonMod, "set_y_axis", active=as.logical(BUTTON_STATUS[["set_y_axis"]]), label="Set y-axis")
  callModule(dynamicClrTxtButtonMod, "show_ppg", active=as.logical(BUTTON_STATUS[["show_ppg"]]),
             default_display=BUTTON_STATUS[["show_ppg_default"]], default_text="Show PPG", updated_text="Remove PPG")
  callModule(dynamicClrButtonMod, "ibi_drag_select", active=as.logical(BUTTON_STATUS[["ibi_drag_select"]]),
             label="Select Mode")
  callModule(dynamicClrButtonMod, "ibi_click_select", active=as.logical(BUTTON_STATUS[["ibi_click_select"]]),
             label="Click Mode")
  callModule(dynamicClrButtonMod, "average", active=as.logical(BUTTON_STATUS[["average"]]), label="Average")
  callModule(dynamicClrButtonMod, "combine", active=as.logical(BUTTON_STATUS[["combine"]]), label="Combine")
  callModule(dynamicClrButtonMod, "divide", active=as.logical(BUTTON_STATUS[["divide"]]), label="Divide")



  # Creating dynamic, updating values for check box...


  # Dynamically Modified Action Button Settings:
#  callModule(dynamicButtonMod, "process_ppg", active=as.logical(BUTTON_STATUS[["process_ppg"]]), label="Process PPG",
#             input_name="process_ppg")

#  callModule(dynamicButtonMod, "ibi_drag_select", active=as.logical(BUTTON_STATUS[["ibi_drag_select"]]),
#             label="Drag Select", input_name="ibi_drag_select")

#  callModule(dynamicButtonMod, "ibi_click_select", active=as.logical(BUTTON_STATUS[["ibi_click_select"]]),
#             label="Click Select", input_name="ibi_click_select")

#  callModule(dynamicButtonMod, "average", active=as.logical(BUTTON_STATUS[["average"]]), label="Average",
#             input_name="average")

#  callModule(dynamicButtonMod, "combine", active=as.logical(BUTTON_STATUS[["combine"]]), label="Combine",
#             input_name="combine")

#  callModule(dynamicButtonMod, "divide", active=as.logical(BUTTON_STATUS[["divide"]]), label="Divide",
#             input_name="divde")

#  callModule(dynamicButtonMod, "uneditable", active=as.logical(BUTTON_STATUS[["uneditable"]]), label="Uneditable",
#             input_name="uneditable")

#  callModule(headUpInfo, "ibi_plots")

}
