FROM alpine
RUN apk add R R-dev g++
COPY install-packages.R install-packages.R
RUN Rscript install-packages.R
RUN del R-dev g++
WORKDIR /workdir
CMD Rscript deploy.R
