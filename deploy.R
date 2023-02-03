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
    appFiles = "app.R"
)

out <- paste(DIR, "Deployed Successfully!")
message(out)
