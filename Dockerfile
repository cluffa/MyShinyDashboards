FROM rocker/shiny-verse
RUN install2.r rsconnect reactable tidyverse
WORKDIR /home/shinyusr
COPY IWF-Data-Explorer/ IWF-Data-Explorer/
COPY Weight-Loss-Trends/ Weight-Loss-Trends/
COPY deploy.R deploy.R
CMD Rscript deploy.R
