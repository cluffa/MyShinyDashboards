FROM rocker/shiny-verse
RUN install2.r rsconnect reactable tidyverse shinydashboard Cairo foreach
WORKDIR /home/shinyusr
COPY . .
CMD Rscript deploy.R
