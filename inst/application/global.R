if(!require('pacman')) install.packages('pacman')
pacman::p_load(shiny, shinythemes, tidyverse, shinyFiles)

AVERAGE_RESPIRATION_BY_AGE <- list(`Young Infant (<1 yr)` = c(30, 60),
                                   `Infant/Toddler (1 to 3 yrs)` =c (24, 40),
                                   `Young Child (3 to 6 yrs)` =c (22, 34),
                                   `Child (6 to 12 yrs)` = c(18, 30),
                                   `Adolescent (12 to 18 yrs)` = c(12, 16),
                                   `Adult (18+ yrs)`= c(12, 20))

CURRENT_NAME_VERSION <- 'ibiVizEdit 0.0.1'
