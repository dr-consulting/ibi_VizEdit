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

idNameFields <- function(id=NULL, label="File ID and Information:"){
  ns <- NS(id)

  tagList(tags$h2(label),
          textInput(ns("sub_id"), "Subject ID:"),
          textInput(ns("secondary_id"), "Time/Task ID:"),
          textInput(ns("optional_id"), "(Optional) Study ID:"),
          textInput(ns("editor"), "Editor Name:"))
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


#' UI utility for \code{ibiVizEdit} that adds "load current settings" actionButton
#'
#' @export

loadSettings <- function(id=NULL, button_color=NULL, label="Load Data Using Current Settings:"){
  ns <- NS(id)

  tagList(tags$h4(label),
          actionButton(ns("load_settings"), label="Load Configuration", style=button_color),
          tags$p("Click to load data with the settings above and begin your editing session."))
}


#' UI utility for \code{ibiVizEdit} that adds "save" actionButton
#'
#' @export

saveProgress <- function(id=NULL, button_color=NULL, label="Save Progress:"){
  ns <- NS(id)

  tagList(tags$h4(label),
          actionButton(ns("save_progress"), label="Save Progress", style=button_color),
          tags$p("Click to save current progress in an .RData object that can be loaded later."))
}


#' UI utility for \code{ibiVizEdit} that adds a "save and finalize" actionButton
#'
#' @export

saveFinalize <-function(id=NULL, button_color=NULL, label="Save and Finalize Outputs:"){
  ns <- NS(id)

  tagList(tags$h4(label),
          actionButton(ns("save_finalize"), label="Save and Output", style=button_color),
          tags$p("Click to save output in finalized format, including report generation, and summary data outputs."))
}


#' UI utility for \code{ibiVizEdit} that adds a "reset" actionButton
#'
#' @export

resetAll <- function(id=NULL, button_color=NULL, label="Return All Settings to Defaults"){
  ns <- NS(id)

  tagList(tags$h4(label),
          actionButton(ns("reset_all"), label="Reset Session", style=button_color),
          tags$p("WARNING: Reset Session will return the session to its default state. All unsaved work will be lost."))
}
