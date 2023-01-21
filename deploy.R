getenv <- function(name){
    var <- Sys.getenv(name, unset = NA)
    if(is.na(var)){
        stop(paste0("cannot find ", name, "!"), call. = FALSE)
    }
    gsub("\"", '',var)
}

TOKEN <- getenv("TOKEN")
SECRET <- getenv("SECRET")
DIR <- getenv("DIR")

deploy <- function(dir) {
    tryCatch(
        {
            library(rsconnect)
            setAccountInfo(
                name = "cluffa",
                token = TOKEN,
                secret = SECRET
            )
            
            deployApp(
                appDir = dir,
                forceUpdate = TRUE,
                launch.browser = FALSE
            )
            
            out <- paste0(dir, "Deployed Successfully")
            message(out)
            return(out)
        },
        error = function(e) {
            message("An Error Occurred")
            return(e)
        },
        warning = function(w) {
            message("A Warning Occurred")
            return(w)
        }
    )
}

deploy(DIR)

