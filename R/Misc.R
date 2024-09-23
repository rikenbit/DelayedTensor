#
# Functions that operateon Matrices and Arrays
#
# list_rep
list_rep <- function(x, n=NULL){
    lapply(seq_len(n), function(a){x})
}

# modebind_list
modebind_list <- function(L, m=NULL){
    # Argument check
    .checkModeBindList(L, m)
    # Setting
    num_modes <- .ndim(L[[1]])
    l <- length(L)
    m_modes <- unlist(lapply(L, function(x,m){dim(x)[m]}, m=m))
    new_modes <- as.integer(dim(L[[1]]))
    new_modes[m] <- sum(m_modes)
    new_modes <- as.integer(new_modes)
    # Vectorization
    Lvec <- lapply(L, function(x){vec(x)})
    cmd <- paste0("Lvec <- rbind(",
        paste(paste0("Lvec[[", seq_len(l), "]]"), collapse=","),
        ")")
    eval(parse(text=cmd))
    Lvec <- .realize_and_return(Lvec)
    Lvec <- .reshapeIncNumbers1D(Lvec, new_modes)
    .realize_and_return(Lvec)
}

# rbind_list
rbind_list <- function(L){
    lapply(L, .check2D)
    modebind_list(L, m=1)
}

# cbind_list
cbind_list <- function(L){
    lapply(L, .check2D)
    modebind_list(L, m=2)
}

# hadamard_list
hadamard_list <- function(L){
    # Argument check
    stopifnot(is.list(L))
    lapply(L, .checkDelayedArray)
    retmat <- L[[1]]
    for(i in 2:length(L)){
        retmat <- .hadamard(retmat, L[[i]])
    }
    as(retmat, "DelayedArray")
}

# kronecker_list
kronecker_list <- function(L){
    # Argument check
    stopifnot(is.list(L))
    lapply(L, .checkDelayedArray)
    retmat <- L[[1]]
    for(i in 2:length(L)){
        retmat <- .kronecker(retmat, L[[i]])
    }
    as(retmat, "DelayedArray")
}

# khatri_rao_list
khatri_rao_list <- function(L, reverse=FALSE){
    # Argument check
    stopifnot(is.list(L))
    stopifnot(is.logical(reverse))
    lapply(L, .checkDelayedArray)
    ncols <- unlist(lapply(L, ncol))
    stopifnot(length(unique(ncols)) == 1)
    if(reverse){
        L <- rev(L)
    }
    retmat <- L[[1]]
    for(i in 2:length(L)){
        retmat <- .khatri_rao(retmat, L[[i]])
    }
    as(retmat, "DelayedArray")
}

# ttl
ttl <- function(darr, list_mat, ms=NULL){
    # Argument Check
    .checkDelayedArray(darr)
    lapply(list_mat, .checkDelayedArray)
    stopifnot(is.list(list_mat))
    if(is.null(ms) || !is.vector(ms)){
        stop ("m modes must be specified as a vector")
    }
    if(length(ms) != length(list_mat)){
        stop("m modes length does not match list_mat length")
    }
    num_mats <- length(list_mat)
    if(length(unique(ms)) != num_mats){
        warning("consider pre-multiplying matrices for the same m for speed")
    }

    # Setting
    mat_nrows <- vector("list", num_mats)
    mat_ncols <- vector("list", num_mats)
    out <- darr
    for(i in seq_len(num_mats)){
        mat <- as(list_mat[[i]], "DelayedMatrix")
        m <- ms[i]
        modes_in <- dim(out)
        mat_dims <- dim(mat)
        stopifnot(modes_in[m] == mat_dims[2])
        modes_out <- modes_in
        modes_out[m] <- mat_dims[1]
        out_m <- .rs_unfold(out, m=m)
        retarr_m <- .realize_and_return(mat %*% out_m)
        out <- .rs_fold(retarr_m, m=m, modes=modes_out)
    }
    out
}

# diag as Constructor
DelayedDiagonalArray <- function(shape, value){
    # Argument Check
    stopifnot(is.vector(shape))
    # Setting
    num_modes <- length(shape)
    min.s <- min(shape)
    if(missing(value)){
        value <- rep(1L, num_modes)
    }
    out <- COO_SparseArray(shape)
    out@nzdata <- as.vector(value)
    out@nzcoo <-  t(vapply(seq_len(min.s),
        function(x){rep(x, num_modes)}, rep(1L, num_modes)))
    DelayedArray(out)
}

# Ref
# https://github.com/rikenbit/rTensor/blob/master/R/rTensor_Misc.R
