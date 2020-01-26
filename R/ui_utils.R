#' UI Utility for \code{ibiVizEdit} that adds dynamic text fields that will update based on user entry
#'
#' @export

dynamicTextInputModUI <- function(id=NULL){
  ns <- NS(id)
  uiOutput(ns("rendered_field"))
}

#' UI utility for \code{ibiVizEdit} that adds dynamic numeric input fields that can update based on user behavior
#'
#' @export

dynamicNumInputModUI <- function(id=NULL){
  ns <- NS(id)
  uiOutput(ns("rendered_field"))
}

#' UI utility for \code{ibiVizEdit} that serves as a basic wrapper and enables single actionButton generation.
#'
#' @export

dynamicClrButtonModUI <- function(id=NULL, inline=FALSE){
  ns <- NS(id)
  uiOutput(ns("rendered_button"), inline=inline)
}


#' UI utility for \code{ibiVizEdit} that serves as a basic wrapper and enables single actionButton generation.
#'
#' @export

dynamicClrTxtButtonModUI <- function(id=NULL, inline=FALSE){
  ns <- NS(id)
  uiOutput(ns("rendered_button"), inline=inline)
}


#' UI utility for \code{ibiVizEdit} that serves as a basic wrapper and enables dynamic updating of checkbox UI
#'
#' @export

dynamicCheckBoxInputModUI <-function(id=NULL){
  ns <- NS(id)
  uiOutput(ns("rendered_checkbox"))
}

#' UI utility for \code{ibiVizEdit} that serves as a basic wrapper and enables dynamic updating of selectInput UI
#'
#' @export

dynamicSelectInputModUI <- function(id=NULL){
  ns <- NS(id)
  uiOutput(ns("rendered_dropdown"))
}

#' UI utility for \code{ibiVizEdit} that renders the Dead Reckoning wide Logo
#'
#' @export

addLogo <- function(logo_filepath=NULL, url="https://www.deadreckoning.consulting/"){
  tags$a(href=url, tags$img(src=logo_filepath))
}

#' UI utility for \code{ibiVizEdit} that generates a series of file and director input objects
#'
#' @export

fileButtons <- function(button_color=BUTTON_COLORS['standard'], heading="Select File(s) & Working Directory"){
  # Add option to load from partially processed data files...

  tagList(tags$h2(heading),
          shinyDirButton("wd_in", label="Select Directory", title="Choose Your Working Directory",
                         style=button_color),
          tags$br(),
          tags$p(textOutput("wd_out")),
          shinyFilesButton("ppg_in", label="Choose PPG File", title="Select Raw PPG .txt File", multiple=FALSE,
                           style=button_color),
          tags$br(),
          tags$p(textOutput("ppg_filepath")),
          shinyFilesButton("timing_in", label="Select Timing File", title="Select (optional) Timing File",
                           multiple=FALSE, style=button_color),
          tags$br(),
          tags$p(textOutput("timing_filepath")),
          tags$hr(),
          tags$h3("Or Load ibiVizEdit Input from .RData File:"),
          shinyFilesButton("rdata_in", label="Select ibiVizEdit Input Data", title="Load ibiVizEdit .RData",
                           multiple = FALSE, style=button_color))
}


#' UI utility for \code{ibiVizEdit} that generates fields for ID, Study, and Editor Labels
#'
#' @export

idNameFields <- function(heading="File ID and Information:"){
  tagList(tags$h2(heading),
          dynamicTextInputModUI("sub_id"),
          dynamicTextInputModUI("secondary_id"),
          dynamicTextInputModUI("optional_id"),
          dynamicTextInputModUI("editor"))
}


#' UI utility for \code{ibiVizEdit} that generates data entry fields for data properties; accepts user-defined defaults
#'
#' @export

ppgDataPropertiesEntry <- function(heading="Data Properties:"){

  tagList(tags$h2(heading),
          dynamicNumInputModUI("column_select"),
          dynamicNumInputModUI("skip_rows"),
          dynamicNumInputModUI("hz_input"),
          dynamicSelectInputModUI("resp_age_grp"))
}


#' UI utility for \code{ibiVizEdit} that generates data entry fields for optional settings
#'
#' @export

optionalSettingsEntry <- function(heading="Optional Settings:"){

  tagList(tags$h2(heading),
          dynamicNumInputModUI("peak_iter"),
          dynamicNumInputModUI("hz_output"),
          dynamicCheckBoxInputModUI("epoch_outputs"))
}


#' UI utility for \code{ibiVizEdit} that generates data entry actionButtons
#'
#' @export
#'

dataEntryActionButtons <- function(){
  fluidRow(
    column(3, dynamicClrButtonModUI("load"),
           tags$p(LOAD_BUTTON_MESSAGE)),
    column(3, dynamicClrButtonModUI("save_progress"),
           tags$p(SAVE_PROG_BUTTON_MESSAGE)),
    column(3, dynamicClrButtonModUI("save_output"),
           tags$p(SAVE_OUT_BUTTON_MESSAGE)),
    column(3, actionButton("hard_reset", label = "Reset Settings", style=BUTTON_COLORS["warning"]),
           tags$p(RESET_ALL_MESSAGE))
  )
}

#' UI utility for \code(ibiVizEdit) that adds a note about open-source nature of program and link to documentation
#'
#' @export

addMainFooter <- function(docs_link=NULL, repo_link=NULL, wiki_link=NULL){
  tags$body(h4("Open-Source Tool for Processing Psychophysiological Data"),
            p(str_wrap("
            This program is intended for use by analysts, researchers, clinicians, and developers interested in
            processing, editing, and obtaining summary information from photoplethysmogram measurements of individuals'
            cardiac activity. While every effort has been made to deliver a reliable product, this software comes with
            no guarantees and is provided as is. The end-user is entirely responsible for conclusions drawn from input
            data passed to the program and the resulting outputs generated. For additional guidance, see the links
            provided below:
            ")),
            p(a(href=docs_link, "ibiVizEdit Manual")),
            p(a(href=repo_link, "GitHub Repository")),
            p(a(href=wiki_link, "Wiki Link (work in progress)")))
}

#' UI utility for \code{ibiVizEdit} that renders a footer for the processing panel
#'
#' @export

addProcessingFooter <- function(heading="Processing Panel Overview"){
  tags$body(h4(heading),
            p(str_wrap("
            The processing panel is the first stage of getting the input PPG data into a form suitable for summarizing
            the timing, frequency, and variability in an individual's cardiac activity. The panel serves two main
            functions. First, after you have loaded the data and configuration settings on the Data Entry tab, you will
            be able to inspect the unprocessed signal in the plot that populates directly above. The lower plot controls
            the section of the signal presented in the upper plot. Simply click and drag open a selection box and the
            contents will display above. You should make sure that you have successfully imported your PPG data before
            hitting the Process Data. If you do not have the desired data, go back to the Data Entry tab, hit Reset All
            and attempt the import again. If you have provided a timing file that delineates tasks or conditions during
            the observation windown it should populate above and to the right. Make sure the timing aligns with
            expectations, and, if not, attempt the import again. The second function this tab performs is to provide a
            basic readout of post-processing data, including the final results of the peak detection algorithm settings,
            and initial estimates of heart rate and respiration. The former will be output in a final case processing
            summary file so you do not need to worry about saving that information. The latter items may change
            based on editing decisions you make by the time you are ready to generate the final outputs. Good luck!!
            ")))
}

#' Utility for \code{ibiVizEdit} that defines plots for pre-processing tab
#'
#' @export

preProcessPlots <- function(heading="Visualize Pre-Processed PPG Data:"){
  tagList(tags$h2(heading),
          plotOutput("pre_process_ppg", height=500),
          plotOutput("pre_process_scroll", height=125, brush=brushOpts("pre_process_x", direction="x")),
          dynamicClrButtonModUI("process_ppg"))
}

#' Utility for \code{ibiVizEdit} that defines tables for pre-processing tab
#'
#' @export

preProcessTables <- function(heading="Task Timing and Peak Detection Outputs:"){
  tagList(tags$h2(heading),
          tags$h4("Timing Data (if present)"),
          tableOutput("task_times"),
          tags$br(),
          tags$h4("Peak Detection Optimization Settings:"),
          tableOutput("peak_detect_tab"))
}


#' Utility for \code{ibiVizEdit} that gemerates UI settings components for ibi editing panel
#'
#' @export

ibiEditingTools <- function(){
  tagList(tags$h4("Heads Up Display:"),
          verbatimTextOutput("heads_up"),
          tags$hr(),
          tags$h4("Plot Settings"),
          sliderInput("y_axis_range", label="Set y-axis min/max:", min=-5, max=5, value=c(0, 2), step=.25),
          fluidRow(
            dynamicClrButtonModUI("set_y_axis", inline=TRUE),
            dynamicClrButtonModUI("show_ppg", inline=TRUE)
          ),
          tags$hr(),
          tags$h4("Editing Mode:"),
          fluidRow(
            dynamicClrButtonModUI("ibi_drag_select", inline=TRUE),
            dynamicClrButtonModUI("ibi_click_select", inline=TRUE)
          ),
          tags$hr(),
          tags$h4("Editing Actions:"),
          fluidRow(
            dynamicClrButtonModUI("average", inline=TRUE),
            dynamicClrButtonModUI("combine", inline=TRUE),
            dynamicClrButtonModUI("divide", inline=TRUE)
          ),
          numericInput("denom", label="Divide by:", min=2, max=6, value=2),
          tags$hr(),
          tags$h4("Special Functions:"),
          fluidRow(
            actionButton("uneditable", label="Unetibale", icon=icon("exclamation-triangle")),
            actionButton("undo", label="Restore IBIs", icon=icon("undo")),
            actionButton("snapshot", label="Take Screenshot", icon=icon("camera"))
          ))
}

#' Utility for \code{ibiVizEdit} that generates UI for basic interporlation and imputation methods
#'
#' @export


#' Utility for \code{ibiVizEdit} that generates UI components for GP Bayesian imputation model parameters
#'
#' @export


#' Utility for \code{ibiVizEdit} that generates UI components for GP Maximum Likelihood imputation model settings
#'
#' @export
