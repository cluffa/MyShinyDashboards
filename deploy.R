p <- c("rsconnect", "reactable", "shinydashboard", "shinyjs", "shinyWidget", "dplyr", "tidyr", "ggplot2", "lubridate", "readr")

install.packages(p)

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
    launch.browser = FALSE
)

out <- paste(DIR, "Deployed Successfully!")
message(out)
