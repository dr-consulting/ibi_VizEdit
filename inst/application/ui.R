source("~/GitHub/IBI_VizEdit/inst/application/global.R")
source("~/GitHub/IBI_VizEdit/R/ui_utils.R")

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
                                 column(3, loadSettings("load_config", BACKGROUND_COLORS['standard'])),
                                 column(3, saveProgress("save", BACKGROUND_COLORS['standard'])),
                                 column(3, saveFinalize("save_and_output", BACKGROUND_COLORS['standard'])),
                                 column(3, resetAll("hard_reset", BACKGROUND_COLORS['warning']))
                               )),
                               fluidRow(
                                 column(4, addLogo(WIDE_LOGO)),
                                 column(8, addMainFooter(DOCS_LINK, REPO_LINK, WIKI_LINK)))),
                      tabPanel(title="Processing Panel",
                               wellPanel(fluidRow(
                                 column(3,)
                               )))))
