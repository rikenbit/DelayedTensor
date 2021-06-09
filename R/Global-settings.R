setSparse <- function(as.sparse=FALSE){
    stopifnot(is.logical(as.sparse))
    options(delayedtensor.sparse = as.sparse)
    if(as.sparse){
        msg <- paste0("All functions except einsum(), outerProd(),\n",
            "and modeSum() have switched to sparse mode.\n")
        message(msg)
    }
}

getSparse <- function(){
    options("delayedtensor.sparse")
}

setVerbose <- function(as.verbose=FALSE){
    stopifnot(is.logical(as.verbose))
    options(delayedtensor.verbose = as.verbose)
}

getVerbose <- function(){
    options("delayedtensor.verbose")
}
