
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
                                 column(9, )))),
                      tabPanel(title="PPG Editing Tab",
                               wellPanel(fluidRow(
                                 column(3, ),
                                 column(9, ))))
                      ))
