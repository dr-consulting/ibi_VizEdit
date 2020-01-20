if(!require('pacman')) install.packages('pacman')
pacman::p_load(shiny, shinythemes, tidyverse, shinyFiles, shinyWidgets)

CURRENT_NAME_VERSION <- 'ibiVizEdit 0.0.1'
TAG_LINE <- "Open-source tool for processing and editing PPG data"

AVERAGE_RESPIRATION_BY_AGE <- list(`Young Infant (<1 yr)`=c(30, 60),
                                   `Infant/Toddler (1 to 3 yrs)`=c(24, 40),
                                   `Young Child (3 to 6 yrs)`=c(22, 34),
                                   `Child (6 to 12 yrs)`=c(18, 30),
                                   `Adolescent (12 to 18 yrs)`=c(12, 16),
                                   `Adult (18+ yrs)`=c(12, 20))
COLUMN_DEFAULT <- 1
SKIP_DEFAULT <- 15
HZ_INPUT_DEFAULT <- 2000
HZ_OUTPUT_DEFAULT <- 1000
DEFAULT_AGE_GROUP <- 3
DEFAULT_PEAK_ITER <- 200
EPOCH_LENGTHS <- c(10, 15, 20, 30, 45)
EPOCH_SELECTED <- EPOCH_LENGTHS # Default is to select all - user could change if they want
WIDE_LOGO <- "dr_logo_wide_40.png"
THUMB_LOGO <- "dr_logo_thumb.png"
DOCS_LINK <- "https://github.com/matgbar/IBI_VizEdit/blob/master/IBI%20VizEdit%20Manual%20v1_2_3.pdf"
REPO_LINK <- "https://github.com/matgbar/IBI_VizEdit"
WIKI_LINK <- "https://github.com/matgbar/IBI_VizEdit/wiki"
BACKGROUND_COLORS <- c(standard="background-color: #426ebd;",
                       inactive="background-color: #a0a9c3;",
                       warning="background-color: #c0392b;")

COLORS <- c(standard="color: #426ebd;",
            inactive="color: #a0a9c3;",
            warning="color: #c0392b;")
