#!/usr/bin/env Rscript

packages <- scan("requirements.txt", character())

library(parallel)
install.packages(
    packages,
    repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest",
    Ncpus = detectCores()
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
