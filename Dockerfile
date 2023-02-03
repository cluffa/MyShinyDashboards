FROM rocker/r-ver
RUN install2.r -n 3 shiny tidyr dplyr lubridate ggplot2 readr rsconnect reactable shinydashboard shinyjs shinyWidgets
WORKDIR /workdir
