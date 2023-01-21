library(rsconnect)

DIR <- getenv("DIR")

setAccountInfo(
    name = "cluffa",
    token = Sys.getenv("TOKEN", unset = NA),
    secret = Sys.getenv("SECRET", unset = NA)
)

deployApp(
    appDir = DIR,
    forceUpdate = TRUE,
    launch.browser = FALSE
)

out <- paste(DIR, "Deployed Successfully!")
message(out)
