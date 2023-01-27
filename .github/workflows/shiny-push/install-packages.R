packages <- c(
    "rsconnect",
    "reactable",

    #"tidyverse",
    "dplyr",
    "tidyr",
    "ggplot2",
    "lubridate",
    "readr",

    "shinydashboard",
    "shinyjs",
    "shinyWidgets"
)

install.packages(packages, repos = "http://cran.us.r-project.org")
lapply(packages, library, character.only = TRUE)
