FROM rocker/shiny
RUN install2.r rsconnect
WORKDIR /home/shinyusr
COPY ./ ./
CMD Rscript deploy.R
