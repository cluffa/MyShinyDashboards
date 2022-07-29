library(rsconnect)

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

deployApp(
  appDir = "IWF-Data-Explorer/",
  forceUpdate = TRUE,
  launch.browser = FALSE
)

deployApp(
  appDir = "Weight-Loss-Trends/",
  forceUpdate = TRUE,
  launch.browser = FALSE
)