library(rsconnect)
library(foreach)

getenv <- function(name){
  var <- Sys.getenv(name, unset=NA)
  if(is.na(var)){
    stop(paste0("cannot find ",name, " !"),call. = FALSE)
  }
  gsub("\"", '',var)
}

setAccountInfo(
  name = "cluffa",
  token = getenv("TOKEN"),
  secret = getenv("SECRET")
)

dirs <- c(
  "IWF-Data-Explorer/",
  "Weight-Loss-Trends/",
  "R6-Stats/"
)


foreach(dir = dirs) %do% {
  deployApp(
    appDir = dir,
    forceUpdate = TRUE,
    launch.browser = FALSE
  )
}