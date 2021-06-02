setSparse <- function(as.sparse=FALSE){
    stopifnot(is.logical(as.sparse))
    options(delayedtensor.sparse = as.sparse)
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
