
fluidPage(theme=shinytheme("cosmo"),
          titlePanel(paste(CURRENT_NAME_VERSION, TAG_LINE, sep=" - ")),
          tabsetPanel(tabPanel(title="Data Entry",
                               wellPanel(fluidRow(
                                 column(3, fileButtons("file_inputs", BACKGROUND_COLORS['standard'])),
                                 column(3, idNameFields("id_entry")),
                                 column(3, ppgDataPropertiesEntry("data_properties", AVERAGE_RESPIRATION_BY_AGE,
                                                                  COLUMN_DEFAULT, SKIP_DEFAULT, HZ_INPUT_DEFAULT,
                                                                  DEFAULT_AGE_GROUP)),
                                 column(3, optionalSettingsEntry("opt_setings", DEFAULT_PEAK_ITER, EPOCH_LENGTHS,
                                                                 EPOCH_SELECTED, HZ_OUTPUT_DEFAULT))
                                 )),
                               wellPanel(fluidRow(
                                 column(3, staticButton("load", BACKGROUND_COLORS["standard"],
                                                        "Load Current Settings", LOAD_BUTTON_MESSAGE)),
                                 column(3, staticButton("save", BACKGROUND_COLORS["standard"],
                                                        "Save Progress", SAVE_PROG_BUTTON_MESSAGE)),
                                 column(3, staticButton("save_and_output", BACKGROUND_COLORS['standard'],
                                                        "Save and Finalize", SAVE_OUT_BUTTON_MESSAGE)),
                                 column(3, staticButton("hard_reset", BACKGROUND_COLORS['warning'], "Reset Session",
                                                        RESET_ALL_MESSAGE))
                               )),
                               fluidRow(
                                 column(4, addLogo(WIDE_LOGO)),
                                 column(8, addMainFooter(DOCS_LINK, REPO_LINK, WIKI_LINK)))),
                      tabPanel(title="Processing Panel",
                               wellPanel(fluidRow(
                                 column(8, basicDualPlots("ppg_preprocess", "Inspect PPG Data before Processing:"),
                                        dynamicButtonModUI("process_pgg")),
                                 column(4, simpleTable("task_data", "Task Timing Below (if Provided)"),
                                        simpleTable("iter_summary", "Peak Detection Iteration Summary:")),
                               )),
                               fluidRow(
                                 column(8, addProcessingFooter()),
                                 column(4, addLogo(WIDE_LOGO)))),
                      tabPanel(title="IBI Editing Pane",
                               wellPanel(fluidRow(
                                 column(3, verbatimTextOutput("heads_up"),
                                        ibiEditingMode(),
                                        tags$hr(),
                                        ibiEditingActions(),
                                        tags$hr(),
                                        ibiSpecialActions()),
                                 column(9, basicDualPlots("ibi_plots")))))))
