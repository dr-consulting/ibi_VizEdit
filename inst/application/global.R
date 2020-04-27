source("~/dr-consulting_GH/ibi_VizEdit/R/ui_utils.R")
source("~/dr-consulting_GH/ibi_VizEdit/R/server_utils.R")
source("~/dr-consulting_GH/ibi_VizEdit/R/general_utils.R")
source("~/dr-consulting_GH/ibi_VizEdit/R/graphing_utilities.R")
source("~/dr-consulting_GH/ibi_VizEdit/R/input_and_process.R")
source("~/dr-consulting_GH/ibi_VizEdit/R/hotkey_utils.R")
source("~/dr-consulting_GH/ibi_VizEdit/R/find_ibis.R")

if(!require('pacman')) install.packages('pacman')
pacman::p_load(shiny, shinythemes, tidyverse, shinyFiles, shinyWidgets, signal, seewave, oce, psych)

# Standard Text Output in UI - Only short, easy to map snippets
CURRENT_NAME_VERSION <- 'ibiVizEdit 0.0.1'
TAG_LINE <- "Open-source tool for processing and editing PPG data"
LOAD_BUTTON_MESSAGE <- "Load selected files and settings."
SAVE_PROG_BUTTON_MESSAGE <- "Save current progress in an .RData file to pick up later"
SAVE_OUT_BUTTON_MESSAGE <- "Save edits and generate all output and summary files"
RESET_ALL_MESSAGE <- "WARNING! Any unsaved work will be lost if you choose to reset"
AVERAGE_RESPIRATION_BY_AGE <- list(`Young Infant (<1 yr)`=c(30,60),
                                   `Infant/Toddler (1 to 3 yrs)`=c(24,40),
                                   `Young Child (3 to 6 yrs)`=c(22,34),
                                   `Child (6 to 12 yrs)`=c(18,30),
                                   `Adolescent (12 to 18 yrs)`=c(12,16),
                                   `Adult (18+ yrs)`=c(12,20))

EPOCH_CHOICES <- c(10, 15, 20, 30, 45)
EPOCH_SELECTED <- c(10, 15, 20, 30, 45)
IBI_POINT_COLORS <- c(edited="#7d3c98",
                      original="#426ebd",
                      uneditable="#c0392b")

# Global variables to be used on both UI and Server Side
WIDE_LOGO <- "dr_logo_wide_33.png"
THUMB_LOGO <- "dr_logo_thumb.png"
DOCS_LINK <- "https://github.com/matgbar/IBI_VizEdit/blob/master/IBI%20VizEdit%20Manual%20v1_2_3.pdf"
REPO_LINK <- "https://github.com/matgbar/IBI_VizEdit"
WIKI_LINK <- "https://github.com/matgbar/IBI_VizEdit/wiki"
BUTTON_COLORS <- c(standard="background-color: #426ebd; border-color: #000000; color: #FFFFFF;",
                   inactive="background-color: #a0a9c3; border-color: #000000; color: #FFFFFF;",
                   warning="background-color: #c0392b; border-color: #000000; color: #FFFFFF;")


EDIT_BUTTON_CLICKS <- c(a=65, c=67, d=68)

# Server-side reactive variables and data_sets
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

META_DATA <- reactiveValues(
  sub_id=NULL,
  secondary_id=NULL,
  optional_id=NULL,
  editor=NULL,
  sys_start_time=NULL,
  sys_end_time=NULL,
  warnings_log=NULL
)

FILE_SETTINGS <- reactiveValues(
  user_dir=NULL,
  max_file=NULL,
  wd=NULL,
  ppg_file=NULL,
  timing_file=NULL
)

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

TRIGGERS <- reactiveValues(
  load=0,
  process_ppg=0,
  set_ibi_y_axis=0,
  show_ppg=0,
  ibi_drag_select=0,
  ibi_click_select=0
)

SUMMARY_STATS <- reactiveValues(
  tot_edits=0,
  mean_bpm=NULL,
  mean_resp=NULL,
  mean_HR_Hz=NULL,
  mean_resp_Hz=NULL
)

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

DYNAMIC_DATA <- reactiveValues(
  edited_ppg=NULL,
  edited_ibi=NULL,
  selected_points=NULL,
  action_log=NULL
)

TEMP_GRAPHICS_SETTINGS <- reactiveValues(
  ymin=NULL,
  ymax=NULL,
  show_ppg=FALSE,
  select_mode="drag"
)
