FROM rocker/r-ver
COPY requirements.txt requirements.txt
RUN install2.r -n -1 $(cat requirements.txt)
WORKDIR /workdir
