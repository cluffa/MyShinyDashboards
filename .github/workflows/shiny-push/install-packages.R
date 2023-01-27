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

install.packages(packages, repos = "https://packagemanager.posit.co/cran")
lapply(packages, library, character.only = TRUE)
