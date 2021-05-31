#
# Generic / Method Definitions
#

# unfold
setGeneric("unfold",
    function(darr, row_idx=NULL, col_idx=NULL){
        standardGeneric("unfold")})
setMethod("unfold", signature(darr="DelayedArray"),
    function(darr, row_idx=NULL, col_idx=NULL){
        .unfold(darr, row_idx, col_idx)})
.unfold <- function(darr, row_idx, col_idx){
    # Argument Check
    .checkUnfold(darr, row_idx, col_idx)
    # Setting
    num_modes <- .ndim(darr)
    perm <- c(row_idx, col_idx)
    modes <- dim(darr)
    new_modes <- as.integer(c(prod(modes[row_idx]), prod(modes[col_idx])))
    mat <- aperm(darr, perm)
    .reshapeIncNumbers1D(vec(mat), new_modes)
    # rearranges into a matrix (only HDF5Array can perform this for now)
    # https://support.bioconductor.org/p/9136602/
    # mat <- aperm(darr, perm)
    # tmpfile <- tempfile()
    # writeHDF5Array(mat, filepath=tmpfile, name="tmp", as.sparse=TRUE)
    # out <- ReshapedHDF5Array(tmpfile, "tmp", new_modes)
    # as(out, "DelayedMatrix")
}

# k_unfold
setGeneric("k_unfold",
    function(darr, m=NULL){
        standardGeneric("k_unfold")})
setMethod("k_unfold", signature(darr="DelayedArray"),
    function(darr, m=NULL){
        .k_unfold(darr, m)})
.k_unfold <- function(darr, m){
    if(is.null(m)){
        stop("mode m must be specified")
    }
    num_modes <- .ndim(darr)
    unfold(darr, row_idx=m, col_idx=seq_len(num_modes)[-m])
}

# matvec
setGeneric("matvec",
    function(darr){
        standardGeneric("matvec")})
setMethod("matvec", signature(darr="DelayedArray"),
    function(darr){
        .matvec(darr)})
.matvec <- function(darr){
    if(.ndim(darr) != 3){
        stop("Matvec currently only implemented for 3d Tensors")
    }
    unfold(darr, row_idx=c(1, 3), col_idx=2)
}

# rs_unfold
setGeneric("rs_unfold",
    function(darr, m=NULL){
        standardGeneric("rs_unfold")})
setMethod("rs_unfold", signature(darr="DelayedArray"),
    function(darr, m=NULL){
        .rs_unfold(darr, m)})
.rs_unfold <- function(darr, m){
    if(is.null(m)){
        stop("mode m must be specified")
    }
    num_modes <- .ndim(darr)
    unfold(darr, row_idx=m, col_idx=seq_len(num_modes)[-m])
}

# cs_unfold
setGeneric("cs_unfold",
    function(darr, m=NULL){
        standardGeneric("cs_unfold")})
setMethod("cs_unfold", signature(darr="DelayedArray"),
    function(darr, m=NULL){
        .cs_unfold(darr, m)})
.cs_unfold <- function(darr, m){
    if(is.null(m)){
        stop("mode m must be specified")
    }
    num_modes <- .ndim(darr)
    unfold(darr, row_idx=seq_len(num_modes)[-m], col_idx=m)
}

# modeSum
setGeneric("modeSum",
    function(darr, m=NULL, drop=FALSE){
        standardGeneric("modeSum")})
setMethod("modeSum", signature(darr="DelayedArray"),
    function(darr, m=NULL, drop=FALSE){
        .modeSum(darr, m, drop)})
.modeSum <- function(darr, m, drop){
    # Argument Check
    .checkModeSum(darr, m)
    # Setting
    num_modes <- .ndim(darr)
    perm <- c(m, (1L:num_modes)[-m])
    revperm <- rep(0, length=num_modes)
    revperm[m] <- 1
    revperm[-m] <- 2:num_modes
    block.size <- getAutoBlockSize()
    # Permutation
    darr_p <- .realize_and_return(aperm(darr, perm))
    p_new_modes <- as.integer(dim(darr_p)[2:num_modes])
    ## darr
    darr_p_spacings <- .blockSizeScheduling3(dim(darr_p), block.size)
    # e.g. 2×3×4×5 => 40 * [2,2,1,1]
    darr_p_grid <- RegularArrayGrid(
        refdim=dim(darr_p),
        spacings=darr_p_spacings)
    ## sink
    sink_grid <- RegularArrayGrid(
        refdim=c(1L, p_new_modes),
        spacings=c(1L, darr_p_spacings[2:num_modes]))
    ## check
    .checkLimit(darr_p_grid, block.size)
    .checkLimit(sink_grid, block.size)
    stopifnot(length(darr_p_grid) %% length(sink_grid) == 0)
    # Block processing
    setAutoRealizationBackend("HDF5Array")
    sink <- AutoRealizationSink(c(1L, p_new_modes))
    for(bid in seq_along(sink_grid)){
        viewport <- sink_grid[[bid]]
        block1 <- read_block(as(sink, "DelayedArray"), viewport)
        block2 <- .block_modesum(darr_p, darr_p_grid, bid)
        sink <- write_block(sink, viewport, block1 + block2)
        cat(paste0("\\ Processing viewport ",
            bid, "/", length(sink_grid),
            " ... OK\n"))
    }
    close(sink)
    out <- as(sink, "DelayedArray")
    # reshape
    out <- .realize_and_return(aperm(out, revperm))
    if(drop){
        # drop
        dif <- setdiff(seq_len(num_modes), m)
        out <- .realize_and_return(aperm(out, dif))
    }
    out
}

.block_modesum <- function(darr_p, darr_p_grid, bid){
    # sink: bid = 1,2,3,4,...,length(sink_grid)
    # darr: did = (1,2,3), (4,5,6), (7,8,9),...,(start:end)
    # Summation range
    stepsize <- dim(darr_p_grid)[1]
    start <- as.integer((bid - 1) * stepsize + 1)
    end <- as.integer(bid * stepsize)
    out <- read_block(darr_p, darr_p_grid[[start]])
    out[] <- 0
    for(did in start:end){
        out <- out + read_block(darr_p, darr_p_grid[[did]])
    }
    .block_collapse(out)
}

.block_collapse <- function(a){
    num_modes <- .ndim(a)
    commas <- paste(rep("", length=num_modes), collapse=",")
    m <- array(0, dim=c(1, dim(a)[2:num_modes]))
    cmd <- paste0("for(z in seq_len(dim(a)[[1]])){",
        "m[1", commas, "] <- ",
        "m[1", commas, "] + a[z", commas, "]}")
    eval(parse(text=cmd))
    m
}

# modeMean
setGeneric("modeMean",
    function(darr, m=NULL, drop=FALSE){
        standardGeneric("modeMean")})
setMethod("modeMean", signature(darr="DelayedArray"),
    function(darr, m=NULL, drop=FALSE){
        .modeMean(darr, m, drop)})
.modeMean <- function(darr, m, drop){
    modes <- dim(darr)
    .realize_and_return(.modeSum(darr, m, drop) / modes[m])
}

# fnorm
setGeneric("fnorm",
    function(darr){
        standardGeneric("fnorm")})
setMethod("fnorm", signature(darr="DelayedArray"),
    function(darr){
        .fnorm(darr)})
.fnorm <- function(darr){
    sqrt(.innerProd(darr, darr))
}

# innerProd
setGeneric("innerProd",
    function(darr1, darr2){
        standardGeneric("innerProd")})
setMethod("innerProd", signature(darr1="DelayedArray", darr2="DelayedArray"),
    function(darr1, darr2){
        .innerProd(darr1, darr2)})
.innerProd <- function(darr1, darr2){
    stopifnot(identical(dim(darr1), dim(darr2)))
    sum(darr1 * darr2)
}

# vec
setGeneric("vec",
    function(darr){
        standardGeneric("vec")})
setMethod("vec", signature(darr="DelayedArray"),
    function(darr){
        .vec(darr)})
.vec <- function(darr){
    l <- as.integer(prod(dim(darr)))
    tmpfile <- paste0(tempfile(), ".h5")
    writeHDF5Array(darr, tmpfile, "tmp")
    out <- ReshapedHDF5Array(tmpfile, "tmp", l)
    as(out, "DelayedArray")
}

# hadamard
setGeneric("hadamard",
    function(darr1, darr2){
        standardGeneric("hadamard")})
setMethod("hadamard",
    signature(darr1="DelayedArray", darr2="DelayedArray"),
    function(darr1, darr2){
        .hadamard(darr1, darr2)})
.hadamard <- function(darr1, darr2){
    # Argument Check
    .checkHadamard(darr1, darr2)
    ## Setting
    mode1s <- as.integer(dim(darr1))
    mode2s <- as.integer(dim(darr2))
    new_modes <- mode1s
    block.size <- getAutoBlockSize()
    ## sink
    sink_spacings <- .blockSizeScheduling(new_modes, block.size)
    sink_grid <- RegularArrayGrid(
        refdim=new_modes,
        spacings=sink_spacings)
    ## check
    .checkLimit(sink_grid, block.size)
    # Block processing
    setAutoRealizationBackend("HDF5Array")
    sink <- AutoRealizationSink(new_modes)
    FUN <- function(sink_viewport, sink) {
        bid <- currentBlockId()
        block <- .block_hadamard(darr1, darr2, sink_grid, bid)
        write_block(sink, sink_viewport, block)
    }
    sink <- gridReduce(FUN, sink_grid, sink, verbose=TRUE)
    close(sink)
    as(sink, "DelayedArray")
}

.block_hadamard <- function(darr1, darr2, sink_grid, bid){
    a <- read_block(darr1, sink_grid[[bid]])
    b <- read_block(darr2, sink_grid[[bid]])
    # Hadamard product
    a <- as.array(a)
    b <- as.array(b)
    DelayedArray(a * b)
}

# kronecker
setGeneric("kronecker",
    function(darr1, darr2){
        standardGeneric("kronecker")})
setMethod("kronecker",
    signature(darr1="DelayedArray", darr2="DelayedArray"),
    function(darr1, darr2){
        .kronecker(darr1, darr2)})
.kronecker <- function(darr1, darr2){
    # Argument Check
    .checkKronecker(darr1, darr2)
    # Setting
    mode1s <- as.integer(dim(darr1))
    mode2s <- as.integer(dim(darr2))
    new_modes <- as.integer(mode1s * mode2s)
    block.size <- getAutoBlockSize()
    ## darr2
    darr_spacings2 <- .blockSizeScheduling(mode2s, block.size)
    darr_grid_2 <- RegularArrayGrid(
        refdim=mode2s,
        spacings=darr_spacings2)
    ## darr1
    darr_spacings1 <- .blockSizeScheduling2(mode1s, mode2s, block.size)
    darr_grid_1 <- RegularArrayGrid(
        refdim=mode1s,
        spacings=darr_spacings1)
    ## sink
    tickmarks <- .tickMarksScheduling1(new_modes, darr_grid_1, darr_grid_2)
    sink_grid <- ArbitraryArrayGrid(tickmarks = tickmarks)
    block_modes <- dim(darr_grid_1) * dim(darr_grid_2)
    ## check
    .checkLimit(darr_grid_1, block.size)
    .checkLimit(darr_grid_2, block.size)
    .checkLimit(sink_grid, block.size)
    stopifnot(length(darr_grid_1) * length(darr_grid_2) ==
        length(sink_grid))
    # Block processing
    setAutoRealizationBackend("HDF5Array")
    sink <- AutoRealizationSink(new_modes)
    for(bid in seq_along(sink_grid)){
        viewport <- sink_grid[[bid]]
        block <- .block_kronecker(darr1, darr2,
            darr_grid_1, darr_grid_2, bid, block_modes)
        sink <- write_block(sink, viewport, block)
        cat(paste0("\\ Processing viewport ",
            bid, "/", length(sink_grid),
            " ... OK\n"))
    }
    close(sink)
    as(sink, "DelayedArray")
}

.block_kronecker <- function(darr1, darr2, darr_grid_1, darr_grid_2,
    bid, block_modes){
    # Indexing
    idx1_idx2 <- .IDX(bid, block_modes, darr_grid_1, darr_grid_2)
    idx1 <- idx1_idx2$idx1
    idx2 <- idx1_idx2$idx2
    # Kronecker product
    a <- read_block(darr1, darr_grid_1[[idx1]])
    b <- read_block(darr2, darr_grid_2[[idx2]])
    a <- as.array(a)
    b <- as.array(b)
    DelayedArray(base::kronecker(a, b))
}

.IDX <- function(bid, block_modes, darr_grid_1, darr_grid_2){
    Mindex <- as.vector(Lindex2Mindex(bid, block_modes))
    idx2 <- Mindex %% dim(darr_grid_2)
    zero_idx <- which(idx2 == 0)
    idx2[zero_idx] <- dim(darr_grid_2)[zero_idx]
    idx1 <- (Mindex - idx2) / dim(darr_grid_2) + 1
    idx1 <- Mindex2Lindex(idx1, dim(darr_grid_1))
    idx2 <- Mindex2Lindex(idx2, dim(darr_grid_2))
    idx1 <- as.integer(idx1)
    idx2 <- as.integer(idx2)
    list(idx1=idx1, idx2=idx2)
}

# khatri_rao
setGeneric("khatri_rao",
    function(darr1, darr2){
        standardGeneric("khatri_rao")})
setMethod("khatri_rao",
    signature(darr1="DelayedArray", darr2="DelayedArray"),
    function(darr1, darr2){
        .khatri_rao(darr1, darr2)})
.khatri_rao <- function(darr1, darr2){
    # Argument Check
    .checkKhatri_Rao(darr1, darr2)
    # Setting
    mode1s <- as.integer(dim(darr1))
    mode2s <- as.integer(dim(darr2))
    new_modes <- as.integer(c(mode1s[1]*mode2s[1], mode1s[2]))
    block.size <- getAutoBlockSize()
    ## darr2
    darr_spacings2 <- .blockSizeScheduling(mode2s, block.size)
    darr_grid_2 <- RegularArrayGrid(
        refdim=dim(darr2),
        spacings=darr_spacings2)
    ## darr1
    darr_spacings1 <- .blockSizeScheduling2(mode1s, mode2s, block.size)
    darr_spacings1[2] <- darr_spacings2[2]
    darr_grid_1 <- RegularArrayGrid(
        refdim=dim(darr1),
        spacings=darr_spacings1)
    ## sink
    tickmarks <- .tickMarksScheduling2(new_modes, darr_grid_1, darr_grid_2)
    sink_grid <- ArbitraryArrayGrid(tickmarks = tickmarks)
    block_modes <- c(dim(darr_grid_1)[1] * dim(darr_grid_2)[1],
                    dim(darr_grid_1)[2])
    ## check
    .checkLimit(darr_grid_1, block.size)
    .checkLimit(darr_grid_2, block.size)
    .checkLimit(sink_grid, block.size)
    stopifnot(ncol(darr_grid_1) == ncol(sink_grid))
    stopifnot(ncol(darr_grid_2) == ncol(sink_grid))
    stopifnot(length(darr_grid_1) <= length(sink_grid))
    stopifnot(length(darr_grid_2) <= length(sink_grid))
    # Block processing
    setAutoRealizationBackend("HDF5Array")
    sink <- AutoRealizationSink(new_modes)
    for (bid in seq_along(sink_grid)) {
        viewport <- sink_grid[[bid]]
        block <- .block_khatri_rao(darr1, darr2,
            darr_grid_1, darr_grid_2, bid, block_modes)
        sink <- write_block(sink, viewport, block)
        cat(paste0("\\ Processing viewport ",
            bid, "/", length(sink_grid),
            " ... OK\n"))
    }
    close(sink)
    as(sink, "DelayedArray")
}

.block_khatri_rao <- function(darr1, darr2, darr_grid_1, darr_grid_2,
    bid, block_modes){
    # Indexing
    idx1_idx2 <- .IDX2(bid, block_modes, darr_grid_1, darr_grid_2)
    idx1 <- idx1_idx2$idx1
    idx2 <- idx1_idx2$idx2
    # Khatri-Rao product
    a <- read_block(darr1, darr_grid_1[[idx1]])
    b <- read_block(darr2, darr_grid_2[[idx2]])
    stopifnot(ncol(a) == ncol(b))
    a <- as.array(a)
    b <- as.array(b)
    out <- matrix(0, nrow=nrow(a)*nrow(b), ncol=ncol(a))
    for(i in seq_len(ncol(a))){
        out[,i] <- base::kronecker(as.matrix(a[,i]), as.matrix(b[,i]))
    }
    DelayedArray(out)
}

.IDX2 <- function(bid, block_modes, darr_grid_1, darr_grid_2){
    Mindex <- as.vector(Lindex2Mindex(bid, block_modes))
    idx2 <- Mindex[1] %% dim(darr_grid_2)[1]
    if(idx2 == 0){
        idx2 = dim(darr_grid_2)[1]
    }
    idx1 <- (Mindex[1] - idx2) / dim(darr_grid_2)[1] + 1
    idx1 <- Mindex2Lindex(c(idx1, Mindex[2]), dim(darr_grid_1))
    idx2 <- Mindex2Lindex(c(idx2, Mindex[2]), dim(darr_grid_2))
    idx1 <- as.integer(idx1)
    idx2 <- as.integer(idx2)
    list(idx1=idx1, idx2=idx2)
}

# fold
setGeneric("fold",
    function(mat, row_idx=NULL, col_idx=NULL, modes=NULL){
        standardGeneric("fold")})
setMethod("fold",
    signature(mat="DelayedArray"),
    function(mat, row_idx=NULL, col_idx=NULL, modes=NULL){
        .fold(mat, row_idx, col_idx, modes)})
.fold <- function(mat, row_idx, col_idx, modes){
    # Argument Check
    .checkFold(mat, row_idx, col_idx, modes)
    # Setting
    num_modes <- length(modes)
    # Reshaping
    # This part might be implemented by ReshapedHDF5Array in near future
    # new_modes <- c(modes[row_idx], modes[col_idx])
    # tmpfile <- tempfile()
    # writeHDF5Array(mat, filepath=tmpfile, name="tmp", as.sparow_idxe=TRUE)
    # sink <- ReshapedHDF5Array(tmpfile, "tmp", new_modes)
    new_modes <- as.integer(c(modes[row_idx], modes[col_idx]))
    # sink <- .reshapeIncNumbers2D(mat, new_modes)
    sink <- .reshapeIncNumbers1D(vec(mat), new_modes)
    #rearranges into array
    iperm <- match(seq_len(num_modes), c(row_idx, col_idx))
    sink <- aperm(sink, iperm)
    .realize_and_return(sink)
}

.block_reshape <- function(x, dim){
    out <- array(0, dim)
    out[] <- as.vector(x)
    out
}

# k_fold
setGeneric("k_fold",
    function(mat, m=NULL, modes=NULL){
        standardGeneric("k_fold")})
setMethod("k_fold",
    signature(mat="DelayedArray"),
    function(mat, m=NULL, modes=NULL){
        .k_fold(mat, m, modes)})
.k_fold <- function(mat, m, modes){
    if(is.null(m)){
        stop("mode m must be specified")
    }
    if(is.null(modes)){
        stop("DelayedTensor modes must be specified")
    }
    num_modes <- length(modes)
    .fold(mat, row_idx=m, col_idx=seq_len(num_modes)[-m], modes=modes)
}

# unmatvec
setGeneric("unmatvec",
    function(mat, modes=NULL){
        standardGeneric("unmatvec")})
setMethod("unmatvec",
    signature(mat="DelayedArray"),
    function(mat, modes=NULL){
        .unmatvec(mat, modes)})
.unmatvec <- function(mat, modes){
    if(is.null(modes)){
        stop("DelayedTensor modes must be specified")
    }
    num_modes <- length(modes)
    .fold(mat, row_idx=seq_len(num_modes)[-2], col_idx=2, modes=modes)
}

# rs_fold
setGeneric("rs_fold",
    function(mat, m=NULL, modes=NULL){
        standardGeneric("rs_fold")})
setMethod("rs_fold",
    signature(mat="DelayedArray"),
    function(mat, m=NULL, modes=NULL){
        .rs_fold(mat, m, modes)})
.rs_fold <- function(mat, m, modes){
    if(is.null(m)){
        stop("mode m must be specified")
    }
    if(is.null(modes)){
        stop("DelayedTensor modes must be specified")
    }
    num_modes <- length(modes)
    .fold(mat, row_idx=m, col_idx=seq_len(num_modes)[-m], modes=modes)
}

# cs_fold
setGeneric("cs_fold",
    function(mat, m=NULL, modes=NULL){
        standardGeneric("cs_fold")})
setMethod("cs_fold",
    signature(mat="DelayedArray"),
    function(mat, m=NULL, modes=NULL){
        .cs_fold(mat, m, modes)})
.cs_fold <- function(mat, m, modes){
    if(is.null(m)){
        stop("mode m must be specified")
    }
    if(is.null(modes)){
        stop("DelayedTensor modes must be specified")
    }
    num_modes <- length(modes)
    .fold(mat, row_idx=seq_len(num_modes)[-m], col_idx=m, modes=modes)
}

# ttm
setGeneric("ttm",
    function(darr, mat, m=NULL){
        standardGeneric("ttm")})
setMethod("ttm",
    signature(darr="DelayedArray", mat="DelayedArray"),
    function(darr, mat, m=NULL){
        .ttm(darr, mat, m)})
.ttm <- function(darr, mat, m){
    # check
    mat_dims <- dim(mat)
    modes_in <- dim(darr)
    stopifnot(modes_in[m] == mat_dims[2])
    modes_out <- modes_in
    modes_out[m] <- mat_dims[1]
    darr_m <- .rs_unfold(darr, m=m)
    retarr_m <- .realize_and_return(mat %*% darr_m)
    .rs_fold(retarr_m, m=m, modes=modes_out)
}

# diag as getter
setGeneric("diag",
    function(darr){
        standardGeneric("diag")})
setMethod("diag", signature(darr="DelayedArray"),
    function(darr){
        .diag(darr)})
.diag <- function(darr){
    num_modes <- .ndim(darr)
    len.i <- min(dim(darr))
    out <- RandomBinomArray(len.i, size=0, prob=0)
    cmd <- paste0("for(i in seq_len(len.i)){",
        "out[i] <- darr[",
            paste(rep("i", length=num_modes), collapse=","), "]}")
    eval(parse(text=cmd))
    .realize_and_return(out)
}

# diag as setter
setGeneric("diag<-",
    function(darr, value){
        standardGeneric("diag<-")})
setMethod("diag<-", signature(darr="DelayedArray"),
    function(darr, value){
        `diag<-`(darr, value)})
`diag<-` <- function(darr, value){
    num_modes <- .ndim(darr)
    len.i <- min(dim(darr))
    len.v <- length(value)
    if(len.i == len.v){
        cmd <- "] <- value[i]}"
    }else if(len.v == 1){
        cmd <- "] <- value}"
    }else{
        stop("replacement diagonal has wrong length")
    }
    cmd <- paste0("for(i in seq_len(len.i)){darr[",
        paste(rep("i", length=num_modes), collapse=","),
        cmd)
    eval(parse(text=cmd))
    .realize_and_return(darr)
}

# Ref
# https://github.com/rikenbit/rTensor/blob/master/R/rTensor_Class.R