source("~/GitHub/IBI_VizEdit/R/ui_utils.R")
source("~/GitHub/IBI_VizEdit/R/server_utils.R")
source("~/GitHub/IBI_VizEdit/R/general_utils.R")
source("~/GitHub/IBI_VizEdit/R/graphing_utilities.R")

if(!require('pacman')) install.packages('pacman')
pacman::p_load(shiny, shinythemes, tidyverse, shinyFiles, shinyWidgets)

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
IBI_POINT_COLORS <- c("edited"="#7d3c98",
                      "original"="#426ebd",
                      "uneditable"="#c0392b")

# Global variables to be used on both UI and Server Side

COLUMN_DEFAULT <- 1
SKIP_DEFAULT <- 15
HZ_INPUT_DEFAULT <- 2000
HZ_OUTPUT_DEFAULT <- 1000
DEFAULT_AGE_GROUP <- 3
DEFAULT_PEAK_ITER <- 200
EPOCH_LENGTHS <- c(10, 15, 20, 30, 45)
EPOCH_SELECTED <- EPOCH_LENGTHS # Default is to select all - user could change if they want
WIDE_LOGO <- "dr_logo_wide_33.png"
THUMB_LOGO <- "dr_logo_thumb.png"
DOCS_LINK <- "https://github.com/matgbar/IBI_VizEdit/blob/master/IBI%20VizEdit%20Manual%20v1_2_3.pdf"
REPO_LINK <- "https://github.com/matgbar/IBI_VizEdit"
WIKI_LINK <- "https://github.com/matgbar/IBI_VizEdit/wiki"
BUTTON_COLORS <- c(standard="background-color: #426ebd; border-color: #000000; color: #FFFFFF;",
                   inactive="background-color: #a0a9c3; border-color: #000000; color: #FFFFFF;",
                   warning="background-color: #c0392b; border-color: #000000; color: #FFFFFF;")

# Server-side reactive variables and data_sets
PROCESSING_SETTINGS <- reactiveValues(
  column_select=1,
  skip_rows=15,
  hz_input=2000,
  hz_output=1000,
  age_group_select=3,
  peak_iter=200,
  epoch_choices=EPOCH_CHOICES,
  epoch_selected=EPOCH_SELECTED,
  resp_age_grp_opts = AVERAGE_RESPIRATION_BY_AGE,
  resp_age_grp = 3
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
  max_file=NULL
)

BUTTON_STATUS <- reactiveValues(
  load=0,
  save_progress=0,
  save_output=0,
  process_ppg=0,
  show_ppg=0,
  ibi_drag_select=0,
  ibi_click_select=1,
  average=1,
  combine=1,
  divide=1,
  uneditable=1
)

SUMMARY_STATS <- reactiveValues(
  tot_edits=0,
  mean_bpm=NULL,
  mean_resp=NULL
)

STATIC_DATA <- reactiveValues(
  orig_Hz=NULL,
  edit_Hz=NULL,
  orig_ppg=NULL,
  processed_ppg=NULL,
  ppg100=NULL,
  orig_ibi=NULL,
  task_times=NULL,
  peak_detect_tab=NULL
)

DYNAMIC_DATA <- reactiveValues(
  edited_PPG=NULL,
  edited_IBI=NULL,
  selected_points=NULL
)

