.svd <- function(dmat, k, ...){
    stopifnot("DelayedMatrix" %in% is(dmat))
    if(options()$delayedtensor.sparse){
        if(k >= min(dim(dmat))){
            stop("Please specify the smaller lower dimension k")
        }else{
            smat <- as(dmat, "sparseMatrix")
            out <- suppressWarnings(irlba(A=smat, nv=k))
        }
    }else{
        if(k >= 0.5 * min(dim(dmat))){
            out <- suppressWarnings(runExactSVD(dmat, k, ...))
        }else{
            out <- suppressWarnings(runIrlbaSVD(dmat, k, ...))
        }
    }
    out
}
