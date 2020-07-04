#' UI utility for \code{ibiVizEdit} that serves as a basic wrapper and enables single actionButton generation.
#'
#' @export

dynamicClrButtonModUI <- function(id=NULL, inline=FALSE){
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
          shinyDirButton("wd", label="Select Directory", title="Choose Your Working Directory",
                         style=button_color),
          tags$br(),
          tags$p(textOutput("wd_out")),
          shinyFilesButton("ppg_file", label="Choose PPG File", title="Select Raw PPG .txt File", multiple=FALSE,
                           style=button_color),
          tags$br(),
          tags$p(textOutput("ppg_file")),
          shinyFilesButton("timing_file", label="Select Timing File", title="Select (optional) Timing File",
                           multiple=FALSE, style=button_color),
          tags$br(),
          tags$p(textOutput("timing_file")),
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
          textInput("sub_id", "Subject ID:"),
          textInput("secondary_id", "Time/Task ID:"),
          textInput("optional_id", "(Optional) Study ID:"),
          textInput("editor", "Editor Name:"))
}


#' UI utility for \code{ibiVizEdit} that generates data entry fields for data properties; accepts user-defined defaults
#'
#' @export

ppgDataPropertiesEntry <- function(heading="Data Properties:"){

  tagList(tags$h2(heading),
          numericInput("column_select", label="PPG Data in Column:", value=PROCESSING_DEFAULTS[["column_select"]]),
          numericInput("skip_rows", label="Number of Header Rows:", value=PROCESSING_DEFAULTS[["skip_rows"]]),
          numericInput("hz_input", label="Original Sampling Rate:", value=PROCESSING_DEFAULTS[["hz_input"]]),
          selectInput("resp_age_grp", label="Age Group:", choices=names(PROCESSING_DEFAULTS[["resp_age_grp_opts"]])))
}


#' UI utility for \code{ibiVizEdit} that generates data entry fields for optional settings
#'
#' @export

optionalSettingsEntry <- function(heading="Optional Settings:"){

  tagList(tags$h2(heading),
          tags$style(HTML("#checkbox :after, #checkbox :before{background-color: #426ebd;}")),
          numericInput("peak_iter", label="Peak Detection Iterations:", value=PROCESSING_DEFAULTS[["peak_iter"]]),
          numericInput("hz_output", label="Hz Output (1000 recomnended):", value=PROCESSING_DEFAULTS[["hz_output"]]),
          awesomeCheckboxGroup("epoch_outputs", label="Select Output Epochs:",
                               choices=PROCESSING_DEFAULTS[["epoch_choices"]],
                               selected=PROCESSING_DEFAULTS[["epoch_selected"]]))
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

addProcessingFooter <- function(heading="Processing Tab Overview"){
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


#' Utility for \code{ibiVizEdit} that gemerates UI components for ibi editing tab tools
#'
#' @export

ibiEditingTools <- function(){
  tagList(tags$h4("Heads Up Display:"),
          verbatimTextOutput("heads_up"),
          tags$hr(),
          tags$h4("Plot Settings"),
          sliderInput("ibi_y_axis", label="Set y-axis min/max:", min=-5, max=5, value=c(0, 2), step=.25),
          fluidRow(
            dynamicClrButtonModUI("set_ibi_y_axis", inline=TRUE),
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
            actionButton("undo_ibi", label="Restore IBIs", icon=icon("undo")),
            actionButton("snapshot_ibi", label="Take Screenshot", icon=icon("camera"))
          ))
}


#' Utility for \code{ibiVizEdit} that generates main editing plots for IBI tab
#'
#' @export

ibiEditingPlots <- function(){
  tagList(plotOutput("ibi_main_plot", height=600, brush=brushOpts("drag_ibis", direction="x"),
                     hover=hoverOpts("hover_ibi", delay=300, delayType = "debounce"), click="click_ibis",
                     dblclick="clear_ibis"),
          plotOutput("ibi_main_scroll", height=125, brush=brushOpts("editing_scroll_x", direction="x")))
}


#' Utility for \code{ibiVizEdit} that generates footer note at the bottom of the IBI tab
#'
#' @export

addIbiFooter <- function(heading="IBI Tab Overview"){
  tags$body(h4(heading),
            p(str_wrap("
            The IBI Editing Tab is the primary interface for you to make adjustments to the interbeat intervals
            returned by the peak detection algorithm. To briefly review each component, the Heads Up Display provides
            summary information about the current editing session and the participants' cardiac activity. You can also
            view specific IBI values by hovering over points in the top window. In the Plot Settings section, you can
            adjust the y-axis to ensure that you edits are made under consistent conditions. You can also choose to
            view a downsampled version of the PPG signal to inform your editing decisions. For Editing Mode, you can
            choose 'Select Mode', in which you target points to edit by clicking and opening selection boxes, or 'Click
            Mode', in which you target points by clicking on them. You can de-select points by double-clicking or
            performing an Editing Action. Editing Actions include Average, Combine, and Divide. Consult the manual and
            Wiki for examples of each. Lastly, you have available a set of special functions, that include marking a
            section of IBIs as 'Uneditable', choosing to 'Restore IBIs' in a portion of the file, or using the 'Take
            Screenshot' button to save an image to file of the current plot window. This latter function was included
            to make it easier to circulate tough editing decisions with colleagues and come to a collective decision.
            ")))
}


#' Utility for \code{ibiVizEdit} that generates UI components for ibi editing tab tools
#'
#' @export

ppgEditingTools <- function(){
  tagList(tags$h4("Plot Settings"),
          sliderInput("ppg_y_axis", label="Set y-axis min/max:", min=-5, max=5, value=c(0, 2), step=.25),
          fluidRow(
            dynamicClrButtonModUI("set_y_axis")
            ),
          tags$hr(),
          tags$h4("Editing Mode:"),
          fluidRow(
            dynamicClrButtonModUI("ppg_edit_mode", inline=TRUE),
            dynamicClrButtonModUI("ppg_imp_mode", inline=TRUE)
            ),
          tags$hr(),
          tags$h4("Editing Actions:"),
          fluidRow(
            dynamicClrButtonModUI("insert", inline=TRUE),
            dynamicClrButtonModUI("remove", inline=TRUE)
          ),
          tags$hr(),
          tags$h4("Imuputation Input Data Tools:"),
          fluidRow(
            dynamicClrButtonModUI("erase_ppg", inline=TRUE),
            dynamicClrButtonModUI("set_impute_window", inline=TRUE),
            dynamicClrButtonModUI("set_valid_ibis", inline=TRUE)
          ),
          tags$hr(),
          tags$h4("Imputation Model Settings:"),
          numericInput("n_iter", label="Total Iterations:", min=1500, max=10000, value=3000),
          numericInput("n_warmup", label="Warmup Iterations:", min=1000, max=9750, value=2500),
          numericInput("adapt_delta", label="Stan `adapt_delta`:", min=.80, max=.999, value=.95),
          tags$p("Consult documentation for use"),
          dynamicClrButtonModUI("gp_impute"),
          tags$hr(),
          fluidRow(
            actionButton("undo_ppg", label="Restore PPG", icon=icon("undo")),
            actionButton("snapshot_ppg", label="Take Screenshot", icon=icon("camera"))
            )
          )
}


#' Utility for \code{ibiVizEdit} that generates main editing plots for PPG tab
#'
#' @export

ppgEditingPlots <- function(){
  tagList(plotOutput("ppg_main_plot", height=600, brush=brushOpts("select_ppg", direction="x"), click="click_ppg",
                     dblclick="dbclick_ppg"),
          plotOutput("ppg_main_scroll", height=125, brush=brushOpts("editing_scroll_x", direction="x")))
}


#' Utility for \code{ibiVizEdit} that generates footer note at the bottom of the PPG tab
#'
#' @export

addPpgFooter <- function(heading="PPG Tab Overview"){
  tags$body(h4(heading),
            p(str_wrap("
            The PPG editing enables two main editing functions. The first is that you can engage 'Insert/Remove' mode
            and manually add in valid IBIs using the PPG waveform as a guide. Simply click as near as possible to a peak
            to add an IBI at that point or double-click on an IBI value present in the series to remove it. The second,
            more advanced tool you can use on this tab is the built-in Gaussian Process imputation model. This tool is
            still in development, so you are advised to use it at your own risk. There are three main steps to using
            the tool currently. First, you need to select the window you want to impute. Then you need to cleanup the
            surrounding signal, using PPG erase to remove any section highlighted in green that does not include
            reliable/valid PPG signal (i.e., any portion that is contaminate by artifacts). Finally, you will have to
            (de-)select all IBIs using a single left-click that fall within the green section you believe are valid. By
            default, `ibiVizEdit`` will select all IBIs within the imputation input range. Once you have finished with
            these steps, you can tweak the imputation model settings (see documentation for more details) and hit 'Run
            Bayesian GPM'. Then kick-back, relax, and wait for the model to run (can range from minutes to hours - as a
            general rule of thumb try not to impute sections longer than 12 seconds).
            ")))
}
