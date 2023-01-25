library(rsconnect)

DIR <- Sys.getenv("DIR")

setAccountInfo("cluffa", Sys.getenv("TOKEN"), Sys.getenv("SECRET"))

deployApp(
    appDir = DIR,
    forceUpdate = TRUE,
    launch.browser = FALSE
)

out <- paste(DIR, "Deployed Successfully!")
message(out)
