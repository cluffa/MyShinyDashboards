FROM dankypants/shinypush
WORKDIR /home/shinyusr
COPY . .
CMD Rscript deploy.R
