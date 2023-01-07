FROM rocker/shiny-verse
RUN install2.r rsconnect reactable tidyverse shinydashboard Cairo foreach doParallel
WORKDIR /home/shinyusr
COPY . .
CMD Rscript deploy.R
