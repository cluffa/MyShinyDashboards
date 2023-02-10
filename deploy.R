#!/usr/bin/env Rscript

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
