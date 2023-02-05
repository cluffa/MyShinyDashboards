#!/usr/bin/env Rscript

packages <- scan("requirements.txt", character())

library(parallel)
cl <- makeCluster(detectCores())

parApply(cl, packages, install.packages)

library(rsconnect)

DIR <- Sys.getenv("DIR")

setAccountInfo(
    name = "cluffa",
    token = Sys.getenv("TOKEN"),
    secret = Sys.getenv("SECRET")
)

deployApp(
    appDir = DIR,
    forceUpdate = TRUE,
    launch.browser = FALSE,
    appFiles = "app.R",
    logLevel = "verbose"
)
