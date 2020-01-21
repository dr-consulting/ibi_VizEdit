source("~/GitHub/IBI_VizEdit/R/server_utils.R")
source("~/GitHub/IBI_VizEdit/R/general_utils.R")

server <- function(input, output, session){
  # Setting Session Timestamp:
  META_DATA[["sys_start_time"]] <- Sys.time()

  # Aquiring correct directory for user folder - intended to be invariant to system
  FILE_SETTINGS[["user_dir"]] <- get_user_folder()
  FILE_SETTINGS[["max_file"]] <- set_file_size_max()

  # Dynamically Modified Action Button Settings:
  callModule(dynamicButtonMod, "process_ppg", active=as.logical(BUTTON_STATUS[["process_ppg"]]), label="Process PPG",
             input_name="process_ppg")

  callModule(dynamicButtonMod, "ibi_drag_select", active=as.logical(BUTTON_STATUS[["ibi_drag_select"]]),
             label="Drag Select", input_name="ibi_drag_select")

  callModule(dynamicButtonMod, "ibi_click_select", active=as.logical(BUTTON_STATUS[["ibi_click_select"]]),
             label="Click Select", input_name="ibi_click_select")

  callModule(dynamicButtonMod, "average", active=as.logical(BUTTON_STATUS[["average"]]), label="Average",
             input_name="average")

  callModule(dynamicButtonMod, "combine", active=as.logical(BUTTON_STATUS[["combine"]]), label="Combine",
             input_name="combine")

  callModule(dynamicButtonMod, "divide", active=as.logical(BUTTON_STATUS[["divide"]]), label="Divide",
             input_name="divde")

  callModule(dynamicButtonMod, "uneditable", active=as.logical(BUTTON_STATUS[["uneditable"]]), label="Uneditable",
             input_name="uneditable")

  callModule(headUpInfo, "ibi_plots")

}
