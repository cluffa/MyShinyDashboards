FROM rocker/shiny
RUN install2.r dplyr
RUN install2.r tidyr
RUN install2.r ggplot2
RUN install2.r lubridate
RUN install2.r readr
RUN install2.r rsconnect
RUN install2.r reactable
RUN install2.r shinydashboard
RUN install2.r shinyjs
RUN install2.r shinyWidgets
WORKDIR /workdir
