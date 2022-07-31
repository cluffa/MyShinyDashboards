FROM rocker/shiny-verse
RUN install2.r rsconnect reactable tidyverse shinydashboard Cairo
WORKDIR /home/shinyusr
COPY IWF-Data-Explorer/ IWF-Data-Explorer/
COPY Weight-Loss-Trends/ Weight-Loss-Trends/
COPY R6/ R6/
COPY deploy.R deploy.R
CMD Rscript deploy.R
