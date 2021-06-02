.svd <- function(dmat, k, ...){
    stopifnot("DelayedMatrix" %in% is(dmat))
    if(options()$delayedtensor.sparse){
        d2s <- dense2sparse(dmat)
        smat <- sparseMatrix(i=d2s@nzindex[,1],
                    j=d2s@nzindex[,2], x=d2s@nzdata)
        irlba(smat, k)
    }else{
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
}
