#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinythemes shinytheme

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(theme=shinytheme("cosmo"),
              titlePanel(paste(CURRENT_NAME_VERSION, TAG_LINE, sep=" - ")),
              tabsetPanel(tabPanel(title="Data Tab",
                                   wellPanel(fluidRow(
                                     column(3, fileButtons()),
                                     column(3, idNameFields()),
                                     column(3, ppgDataPropertiesEntry()),
                                     column(3, optionalSettingsEntry())
                                   )),
                                   wellPanel(dataEntryActionButtons()),
                                   fluidRow(
                                     column(3, addLogo(WIDE_LOGO)),
                                     column(9, addMainFooter(DOCS_LINK, REPO_LINK, WIKI_LINK)))
                                   ),
                          tabPanel(title="Processing Tab",
                                   wellPanel(fluidRow(
                                     column(9, preProcessPlots()),
                                     column(3, preProcessTables())
                                     )),
                                   fluidRow(
                                     column(9, addProcessingFooter()),
                                     column(3, addLogo(WIDE_LOGO)))
                                   ),
                          tabPanel(title="IBI Editing Tab",
                                   wellPanel(fluidRow(
                                     column(3, ibiEditingTools()),
                                     column(9, ibiEditingPlots())
                                     )),
                                   fluidRow(
                                     column(3, addLogo(WIDE_LOGO)),
                                     column(9, addIbiFooter()))
                                   ),
                          tabPanel(title="PPG Editing Tab",
                                   wellPanel(fluidRow(
                                     column(3, ppgEditingTools()),
                                     column(9, ppgEditingPlots())
                                     )),
                                   fluidRow(
                                     column(3, addLogo(WIDE_LOGO)),
                                     column(9, addPpgFooter())))
                          )
              )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources

golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(WIDE_LOGO, ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ibiVizEdit'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

