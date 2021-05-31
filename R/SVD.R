.svd <- function(dmat, k, ...){
    stopifnot("DelayedMatrix" %in% is(dmat))
    oldw <- getOption("warn")
    options(warn = -1)
    if(k >= 0.5 * min(dim(dmat))){
        out <- runExactSVD(dmat, k, ...)
    }else{
        out <- runIrlbaSVD(dmat, k, ...)
    }
    options(warn = oldw)
    out
}
