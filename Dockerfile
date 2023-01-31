FROM rocker/shiny-verse
RUN install2.r rsconnect reactable shinydashboard shinyjs shinyWidgets
WORKDIR /workdir
