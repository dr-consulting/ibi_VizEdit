#' Configuration variable - solely for inclusion in the main header
CURRENT_NAME_VERSION <- 'ibiVizEdit 0.0.1'

#' Configuration setting - Tagline for the application header
TAG_LINE <- "Open-source tool for processing and editing PPG data"

#' Message to display underneath "Load" button
LOAD_BUTTON_MESSAGE <- "Load selected files and settings."

#' Message to display underneath "Save Progress" button
SAVE_PROG_BUTTON_MESSAGE <- "Save current progress in an .RData file to pick up later"

#' Message to display underneath "Save and Output" button
SAVE_OUT_BUTTON_MESSAGE <- "Save edits and generate all output and summary files"

#' Message to display underneath "Reset" button 
RESET_ALL_MESSAGE <- "WARNING! Any unsaved work will be lost if you choose to reset"

#' Prior boundaries based on average respiration rates for each population 
#' 
#' This list is used to define respiration rates and will be incorporated into imputation models to improve accuracy 
#' and to ensure that a sufficient number of respiration cycles are selected. May be subject to manual override in the 
#' future but not in the experimental version. Will also be used when HRV values are calculated as part of the final 
#' set of outputs.
AVERAGE_RESPIRATION_BY_AGE <- list(`Young Infant (<1 yr)`=c(30,60),
                                   `Infant/Toddler (1 to 3 yrs)`=c(24,40),
                                   `Young Child (3 to 6 yrs)`=c(22,34),
                                   `Child (6 to 12 yrs)`=c(18,30),
                                   `Adolescent (12 to 18 yrs)`=c(12,16),
                                   `Adult (18+ yrs)`=c(12,20))

#' Settings for the epochs users can request to have the output "chunked" into
EPOCH_CHOICES <- c(10, 15, 20, 30, 45)

#' Defaults for epoch lengths in output files. 
EPOCH_SELECTED <- c(10, 15, 20, 30, 45)

#' Central location for IBI point colors used throughout the plots based on point type as defined in this named vector
IBI_POINT_COLORS <- c(combined="#e482ff",
                      averaged="#e482ff",
                      divided="#e482ff",
                      original="#426ebd",
                      uneditable="#c0392b")

#' Location of the main Dead Reckoning logo
WIDE_LOGO <- "www/dr_logo_wide_33.png"

#' Location of the thumbnail - used mainly as the favicon
THUMB_LOGO <- "www/dr_logo_thumb"

#' Link to the docs - going to need a significant overhaul
DOCS_LINK <- "https://github.com/matgbar/IBI_VizEdit/blob/master/IBI%20VizEdit%20Manual%20v1_2_3.pdf"

#' Link to the repo 
REPO_LINK <- "https://github.com/dr-consulting/ibi_VizEdit"

#' Link to the wiki - needs to be considerably added to
WIKI_LINK <- "https://github.com/dr-consulting/ibi_VizEdit/wiki"

#' Vector that specifies three basic color templates based on conditions
BUTTON_COLORS <- c(standard="background-color: #426ebd; border-color: #000000; color: #FFFFFF;",
                   inactive="background-color: #a0a9c3; border-color: #000000; color: #FFFFFF;",
                   warning="background-color: #c0392b; border-color: #000000; color: #FFFFFF;")

#' Presets for the inputs 
PROCESSING_DEFAULTS <- list(
  column_select=1,
  skip_rows=15,
  hz_input=2000,
  hz_output=1000,
  age_group_select=3,
  peak_iter=200,
  epoch_choices=EPOCH_CHOICES,
  epoch_selected=EPOCH_SELECTED,
  resp_age_grp_opts=AVERAGE_RESPIRATION_BY_AGE
)

#' Initialization of META_DATA reactiveValues used throughout the application
META_DATA <- reactiveValues(
  sub_id=NULL,
  secondary_id=NULL,
  optional_id=NULL,
  editor=NULL,
  sys_start_time=NULL,
  sys_end_time=NULL,
  warnings_log=NULL
)

#' Initialization of FILE_SETTINGS reactiveValues used throughout the application
FILE_SETTINGS <- reactiveValues(
  user_dir=NULL,
  max_file=NULL,
  wd=NULL,
  ppg_file=NULL,
  timing_file=NULL,
  out_dir=NULL,
  gp_out_dir=NULL,
  screenshot_out_dir=NULL
)

#' Initialization of BUTTON_STATUS reactiveValues used throughout the application
BUTTON_STATUS <- reactiveValues(
  load=0,
  save_progress=0,
  save_output=0,
  process_ppg=0,
  set_ibi_y_axis=0,
  show_ppg=0,
  show_ppg_default=TRUE,
  ibi_drag_select=0,
  ibi_click_select=0,
  average=0,
  combine=0,
  divide=0,
  uneditable=0,
  set_ppg_y_axis=0,
  ppg_edit_mode=0,
  ppg_imp_mode=0,
  insert=0,
  remove=0,
  erase_ppg=0,
  set_impute_window=0,
  set_valid_ibis=0,
  gp_impute=0
)

#' Initiatlization of a set of TRIGGERS reactiveValues used in conjunction with the eventTrigger module
TRIGGERS <- reactiveValues(
  load=0,
  process_ppg=0,
  set_ibi_y_axis=0,
  show_ppg=0,
  ibi_drag_select=0,
  ibi_click_select=0,
  combine=0,
  divide=0,
  average=0
)

#' Initialization of SUMMARY_STATS reactiveValues used to store summary values calculated during application usage
SUMMARY_STATS <- reactiveValues(
  tot_edits=0,
  mean_bpm=NULL,
  mean_resp=NULL,
  mean_HR_Hz=NULL,
  mean_resp_Hz=NULL
)

#' Initialization of STATIC_DATA reactiveValues that, once processed by the application never change
STATIC_DATA <- reactiveValues(
  column_select=NULL,
  skip_rows=NULL,
  hz_input=NULL,
  hz_output=NULL,
  case_id=NULL,
  resp_age_grp=NULL,
  peak_iter=NULL,
  epoch_outputs=NULL,
  orig_ppg=NULL,
  orig_ppg100=NULL,
  processed_ppg=NULL,
  processed_ppg100=NULL,
  orig_ibi=NULL,
  task_times=NULL,
  display_task_times=NULL,
  peak_detect_tab=NULL
)

#' Initialization of DYNAMIC_DATA reactiveValues that can change and be modified based on user actions
DYNAMIC_DATA <- reactiveValues(
  edited_ppg=NULL,
  edited_ibi=NULL,
  selected_points=NULL,
  hover_point=NULL,
  action_log=NULL
)

#' Initialization of TEMP_GRAPHICS_SETTINGS reactiveValues that specify the default settings for certain graphics. All 
#' can be modified by user when interacting with the application. 
TEMP_GRAPHICS_SETTINGS <- reactiveValues(
  ymin=NULL,
  ymax=NULL,
  show_ppg=FALSE,
  select_mode="drag"
)
