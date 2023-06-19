FROM rocker/r-ver:4.2
RUN install2.r -n -1 shiny tidyr dplyr lubridate ggplot2 readr rsconnect reactable shinydashboard shinyjs shinyWidgets stringr
WORKDIR /workdir
