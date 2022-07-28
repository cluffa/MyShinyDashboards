FROM rocker/shiny
RUN install2.r rsconnect
WORKDIR /home/shinyusr
COPY ./ ./
COPY deploy.R deploy.R
CMD Rscript deploy.R
