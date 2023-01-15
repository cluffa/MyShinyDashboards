library(foreach)
library(doParallel)
library(iterators)
library(parallel)

myCluster <- makeCluster(3, type = "PSOCK")
registerDoParallel(myCluster)

getenv <- function(name){
    var <- Sys.getenv(name, unset = NA)
    if(is.na(var)){
        stop(paste0("cannot find ",name, "!"),call. = FALSE)
    }
    gsub("\"", '',var)
}

dirs <- c(
    "IWF-Data-Explorer/",
    "Weight-Loss-Trends/",
    "R6-Stats/"
)

deploy <- function(dir) {
    tryCatch(
        {
            library(rsconnect)
            setAccountInfo(
                name = "cluffa",
                token = getenv("TOKEN"),
                secret = getenv("SECRET")
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

foreach(dir = dirs) %dopar% deploy(dir)

stopCluster(myCluster)

