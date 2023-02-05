#!/usr/bin/env Rscript

packages <- scan("requirements.txt", character())

library(parallel)
install.packages(
    packages,
    repos = Sys.getenv("CRAN")
    Ncpus = detectCores(),
    quiet = TRUE
)

library(rsconnect)

setAccountInfo(
    name = "cluffa",
    token = Sys.getenv("TOKEN"),
    secret = Sys.getenv("SECRET")
)

deployApp(
    appDir = Sys.getenv("DIR"),
    forceUpdate = TRUE,
    launch.browser = FALSE,
    appFiles = "app.R",
    logLevel = "verbose"
)
