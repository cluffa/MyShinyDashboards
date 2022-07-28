FROM rocker/shiny-verse
RUN install2.r rsconnect reactable tidyverse
WORKDIR /home/shinyusr
COPY ./* ./
CMD Rscript deploy.R
