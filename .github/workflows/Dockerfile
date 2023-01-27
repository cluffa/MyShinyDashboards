FROM rocker/r-ver
RUN install2.r rsconnect reactable shinydashboard shinyjs shinyWidget dplyr tidyr ggplot2 lubridate readr
WORKDIR /workdir
CMD Rscript deploy.R
