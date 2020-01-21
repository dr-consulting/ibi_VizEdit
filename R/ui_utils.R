#' UI utility for \code{ibiVizEdit} that adds static actionButton
#'
#' @export

staticButton <- function(id=NULL, button_color=NULL, heading=NULL, msg=NULL){
  TL <- tagList(tags$h4(heading),
                actionButton(id, label="Load Configuration", style=button_color))

  if(!is.null(msg)){
    TL[[3]]<-tags$p(msg)
  }
  return(TL)
}


#' UI utility for \code{ibiVizEdit} that generates a series of file and director input objects
#'
#' @export

fileButtons <- function(id=NULL, button_color=NULL, label="Select File(s) & Working Directory"){
  # Add option to load from partially processed data files...
  ns <- NS(id)

  tagList(tags$h2(label),
          shinyDirButton(id=ns("wd_in"), label="Select Directory", title="Choose Your Working Directory",
                         style=button_color),
          tags$br(),
          tags$p(textOutput(ns("wd_out"))),
          shinyFilesButton(id=ns("ppg_in"), label="Choose PPG File", title="Select Raw PPG .txt File", multiple=FALSE,
                           style=button_color),
          tags$br(),
          tags$p(textOutput(ns("ppg_filepath"))),
          shinyFilesButton(id=ns("timing_in"), label="Select Timing File", title="Select (optional) Timing File",
                           multiple=FALSE, style=button_color),
          tags$br(),
          tags$p(textOutput(ns("timing_filepath"))),
          tags$hr(),
          tags$h3("Or Load ibiVizEdit Input from .RData File:"),
          shinyFilesButton(id=ns("rdata_in"), label="Select ibiVizEdit Input Data", title="Load ibiVizEdit .RData",
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

ppgDataPropertiesEntry <- function(id=NULL, age_cat_mapping=NULL, col_default=NULL, skip_default=NULL,
                                   Hz_in_default=NULL, age_default=NULL,
                                   label="Data Properties:"){
  ns <- NS(id)

  tagList(tags$h2(label),
          numericInput(ns("ppg_col_select"), label="PPG Signal in Column:", min=1, max=999, value=col_default),
          numericInput(ns("ppg_skip_lines"), label="Number of Head Lines:", min=0, max=999, value=skip_default),
          numericInput(ns("Hz_input"), label="File Sampling Rate:", min=125, max=10000, value=Hz_in_default),
          selectInput(ns("sub_age_cat"), label="Select Age Group:", choices=names(age_cat_mapping),
                      selected=names(age_cat_mapping)[age_default]))
}


#' UI utility for \code{ibiVizEdit} that generates dat entry fields for optional settings
#'
#' @export

optionalSettingsEntry <- function(id=NULL, peak_iter=NULL, epoch_lengths=NULL, epoch_default=NULL, Hz_edit_default,
                                  label="Optional Settings:"){
  ns <- NS(id)

  tagList(tags$h2(label),
          numericInput(ns("find_ibi_iter"), label="Peak Detection Iterations:", min=10, max=500, value=peak_iter),
          numericInput(ns("Hz_editing"), label="Signal Hz for Editing:", min=500, max=5000, value=Hz_edit_default),
          awesomeCheckboxGroup(ns("epoch_in"), label="Output Epoch Lengths:", choices=paste0(epoch_lengths, "s"),
                               selected=paste0(epoch_default, "s"), inline=FALSE),
          tags$head(tags$style(HTML("
                                    #checkbox :after, #checkbox :before{
                                    background-color: #426ebd;
                                    }"))))
}

#' UI utility for \code{ibiVizEdit} that renders the Dead Reckoning wide Logo
#'
#' @export

addLogo <- function(logo_filepath=NULL, url="https://www.deadreckoning.consulting/"){
  tags$a(href=url, tags$img(src=logo_filepath))
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


#' UI utility for \code{ibiVizEdit} that renders a pair of stacked plots for signal inspection
#'
#' @export

basicDualPlots <- function(id=NULL, label=NULL, plot1_height=NULL, plot1_click=NULL, plot1_dbclick=NULL,
                           plot1_hover=NULL, plot1_hoverDelay=NULL, plot1_hoverDelayType=NULL, plot1_brush=NULL,
                           plot1_clickId=NULL, plot1_hoverId=NULL, plot2_height=NULL, plot2_click=NULL,
                           plot2_dbclick=NULL, plot2_hover=NULL, plot2_hoverDelay=NULL, plot2_hoverDelayType=NULL,
                           plot2_brush=NULL, plot2_clickId=NULL, plot2_hoverId=NULL){
  ns<-NS(id)

  tagList(tags$h4(label),
          plotOutput(ns("main"), height=plot1_height, click=plot1_click, dblclick=plot1_dbclick, hover=plot1_hover,
                     hoverDelay=plot1_hoverDelay, hoverDelayType=plot1_hoverDelayType, brush=plot1_brush,
                     clickId=plot1_clickId, hoverId=plot1_hoverId),
          plotOutput(ns("scroll_x"), height=plot2_height, click=plot2_click, dblclick=plot2_dbclick, hover=plot2_hover,
                     hoverDelay=plot2_hoverDelay, hoverDelayType=plot2_hoverDelayType, brush=plot2_brush,
                     clickId=plot2_clickId, hoverId=plot2_hoverId))
}


#' UI utility for \code{ibiVizEdit} that renders an Rshiny tableOutput object
#'
#' @export

simpleTable <- function(id=NULL, label=NULL){
  ns<-NS(id)

  tagList(tags$h4(label),
          tableOutput(ns("table")))
}

#' UI utility for \code{ibiVizEdit} that renders a footer for the processing panel
#'
#' @export

addProcessingFooter <- function(label="Processing Panel Overview"){
  tags$body(h4(label),
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


#' UI utility for \code{ibiVizEdit} that serves as a basic wrapper and enables single actionButton generation.
#'
#' @export

dynamicButtonModUI <- function(id=NULL){
  ns <- NS(id)
  uiOutput(ns("rendered_button"))
}


#' Utility for \code{ibiVizEdit} that generates UI components for selection mode drag-select vs. click-and-point
#'
#' @export

ibiEditingMode <- function(names=c("ibi_drag_select", "ibi_click_select"), label="Point Selection Mode:"){
  tagList(tags$h4(label),
          dynamicButtonModUI(names[1]),
          dynamicButtonModUI(names[2]))
}


#' Utility for \code{ibiVizEdit} that generates UI components for basic IBI editing functions
#'
#' @export

ibiEditingActions <- function(names=c("average", "combine", "divide"), heading="Base Editing Functions"){
  tagList(tags$h4(heading),
          dynamicButtonModUI(names[1]),
          dynamicButtonModUI(names[2]),
          dynamicButtonModUI(names[3]),
          numericInput("divisor", label="Divide by:", min=1, max=6, value=2))
}

#' Utility for \code{ibiVizEdit} that generates UI components for summary text display
#'
#' @export

summaryTextOutput <- function(id=NULL){

}

#' Utility for \code{ibiVizEdit} that generates UI components that govern plot display features
#'
#' @export


#' Utility for \code{ibiVizEdit} that generates UI components for assigning uneditable status and restoring IBIs
#'
#' @export
ibiSpecialActions <- function(names=c("uneditable", "restore"), heading="Special Actions:",
                              button_color=BACKGROUND_COLORS["warning"]){
  tagList(tags$h4(heading),
          dynamicButtonModUI(names[1]),
          actionButton(names[2], label="Restore IBI", style=button_color))
}

#' Utility for \code{ibiVizEdit} that generates UI components for PPG plot editing mode
#'
#' @export


#' Utility for \code{ibiVizEdit} that generates UI for basic interporlation and imputation methods
#'
#' @export


#' Utility for \code{ibiVizEdit} that generates UI components for GP Bayesian imputation model parameters
#'
#' @export


#' Utility for \code{ibiVizEdit} that generates UI components for GP Maximum Likelihood imputation model settings
#'
#' @export
