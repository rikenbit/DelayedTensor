# Convert SVT_SparseArray to COO_SparseArray for @nzdata/@nzcoo access
.as_coo <- function(x) {
    if (is(x, "COO_SparseArray")) x else as(x, "COO_SparseArray")
}

.array <- function(dim){
    dim <- as.integer(dim)
    sink <- .create_sink(dim)
    close(sink)
    .sink_to_delayed(sink)
}

# Direct reshape: read all blocks from source into a flat vector,
# then reshape to new_modes in one step.
# Avoids the intermediate vec() HDF5 round-trip.
.reshapeDirect <- function(darr, new_modes){
    block.size <- getAutoBlockSize()
    modes <- dim(darr)
    new_modes <- as.integer(new_modes)
    stopifnot(prod(new_modes) == prod(modes))
    # Grid over source array
    src_spacings <- .blockSizeScheduling(modes, block.size)
    src_grid <- RegularArrayGrid(refdim=modes, spacings=src_spacings)
    # Read all blocks, flatten, concatenate
    chunks <- vector("list", length(src_grid))
    for(bid in seq_along(src_grid)){
        if(options()$delayedtensor.sparse){
            blk <- as.array(read_block(darr, src_grid[[bid]], as.sparse=TRUE))
        }else{
            blk <- read_block(darr, src_grid[[bid]], as.sparse=FALSE)
        }
        chunks[[bid]] <- as.vector(blk)
        if(options()$delayedtensor.verbose){
            cat(paste0("\\ Reading block ",
                bid, "/", length(src_grid), " ... OK\n"))
        }
    }
    flat <- unlist(chunks)
    dim(flat) <- new_modes
    # Write to HDF5-backed DelayedArray
    .realize_and_return(DelayedArray(flat))
}

# for fold, unfold, and modebind_list (legacy, kept for compatibility)
.reshapeIncNumbers1D <- function(vecobj, new_modes){
    # Setting
    block.size <- getAutoBlockSize()
    dim(vecobj) <- c(dim(vecobj), 1) # Fake dimension
    num_modes <- length(new_modes)
    # Block Size Scheduling
    sink_spacings <- .blockSizeScheduling4(new_modes, num_modes, block.size)
    # e.g. 9×2=18 => [9×1], [9×1]
    sink_grid <- RegularArrayGrid(
        refdim=new_modes,
        spacings=sink_spacings)
    # Block Size Scheduling
    # e.g. 18 => [9], [9]
    tickmarks <- unlist(lapply(sink_grid, function(x){
        prod(dim(x))
    }))
    tickmarks <- list(as.integer(cumsum(tickmarks)), 1L)
    vec_grid <- ArbitraryArrayGrid(tickmarks = tickmarks)
    # Check
    .checkLimit(sink_grid, block.size)
    .checkLimit(vec_grid, block.size)
    stopifnot(identical(length(sink_grid), length(vec_grid)))
    # Block processing
    sink <- .create_sink(new_modes)
    FUN <- function(sink_viewport, sink) {
        bid <- currentBlockId()
        block <- .block_reshape(vecobj, vec_grid, bid, sink_viewport)
        write_block(sink, sink_viewport, block)
    }
    sink <- gridReduce(FUN, sink_grid, sink,
        verbose=options()$delayedtensor.verbose)
    close(sink)
    .sink_to_delayed(sink)
}

.block_reshape <- function(vecobj, vec_grid, bid, sink_viewport){
    dim_sink_viewport <- as.integer(dim(sink_viewport))
    if(options()$delayedtensor.sparse){
        v <- as.array(read_block(vecobj, vec_grid[[bid]], as.sparse=TRUE))
        dim(v) <- dim_sink_viewport
        v
    }else{
        v <- read_block(vecobj, vec_grid[[bid]], as.sparse=FALSE)
        dim(v) <- dim_sink_viewport
        v
    }
}

# Block Size Scheduling
.zeros <- function(n){
    rep(0, length=n)
}

# for hadamard, kronecker, khatri_rao
.blockSizeScheduling <- function(modes, block.size){
    num_modes <- length(modes)
    spacings <- .zeros(num_modes)
    for(i in seq_len(num_modes)){
        limit <- prod(modes[seq_len(i)])
        if(limit <= block.size){
            spacings[i] <- modes[i]
        }else{
            spacings[i] <- max(1, floor(block.size/(limit/modes[i])))
        }
    }
    spacings
}

# for kronecker, khatri_rao
.blockSizeScheduling2 <- function(modes1, modes2, block.size){
    num_modes <- length(modes1)
    spacings <- .zeros(num_modes)
    for(i in seq_len(num_modes)){
        limit <- prod(modes2) * prod(modes1[seq_len(i)])
        if(limit <= block.size){
            spacings[i] <- modes1[i]
        }else{
            spacings[i] <- max(1, floor(block.size/(limit/modes1[i])))
        }
    }
    spacings
}

# for .colSums
.blockSizeScheduling3 <- function(modes, block.size){
    num_modes <- length(modes)
    spacings <- .zeros(num_modes)
    for(i in seq_len(num_modes)){
        limit <- prod(modes[seq_len(i)])
        if(limit <= block.size){
            spacings[i] <- modes[i]
        }else{
            if(i == 1){
                idx <- 1
            }else{
                idx <- seq_len(i-1)
            }
            spacings[i] <- max(1,
                floor(block.size / prod(modes[idx])))
        }
    }
    spacings
}

# for .reshapeIncNumbers1D
.blockSizeScheduling4 <- function(new_modes, num_modes, block.size){
    spacings <- rep(0, length=num_modes)
    for(i in seq_len(num_modes)){
        limit <- prod(new_modes[seq_len(i)])
        if(limit <= block.size){
            spacings[i] <- new_modes[i]
        }else{
            if(i == 1){
                idx <- 1
            }else{
                idx <- seq_len(i-1)
            }
            # spacings[i] <- 1
            spacings[i] <- max(1, floor(block.size / prod(new_modes[idx])))
        }
    }
    spacings
}

# for .kronecker
.tickMarksScheduling1 <- function(new_modes, darr_grid_1, darr_grid_2){
    lapply(seq_along(new_modes), function(xx){
            dim_1 <- unlist(
                gridApply(darr_grid_1,
                    function(x){dim(x)[xx]}))[
                        seq_len(dim(darr_grid_1)[xx])]
            dim_2 <- unlist(
                gridApply(darr_grid_2,
                    function(x){dim(x)[xx]}))[
                        seq_len(dim(darr_grid_2)[xx])]
            out <- vapply(dim_1, function(x){
                dim_2 * x
            }, dim_2)
            out <- as.vector(out)
            out <- cumsum(out)
            as.integer(out)
        })
}

# for .khatri_rao
.tickMarksScheduling2 <- function(new_modes, darr_grid_1, darr_grid_2){
    # Row block size
    dim1_1 <- unlist(
        gridApply(darr_grid_1,
            function(x){dim(x)[1]}))[
                seq_len(dim(darr_grid_1)[1])]
    dim2_1 <- unlist(
        gridApply(darr_grid_2,
            function(x){dim(x)[1]}))[
                seq_len(dim(darr_grid_2)[1])]
    row_tickmark <- vapply(dim1_1, function(x){
        dim2_1 * x
    }, dim2_1)
    row_tickmark <- cumsum(row_tickmark)
    row_tickmark <- as.integer(row_tickmark)
    # Column block size
    col_index <- Mindex2Lindex(
        cbind(1, seq_len(dim(darr_grid_1)[2])), dim(darr_grid_1))
    col_tickmark <- vapply(col_index, function(x){
        dim(darr_grid_1[[x]])[2]
    }, 0L)
    col_tickmark <- cumsum(col_tickmark)
    col_tickmark <- as.integer(col_tickmark)
    # Output
    list(row_tickmark, col_tickmark)
}

.ndim <- function(x){
    length(dim(x))
}

.checkHOSVD <- function(ranks, darr){
    if(sum(ranks <= 0) != 0){
        stop("ranks must be positive")
    }
    if(.is_zero_tensor(darr)){
        stop("Zero tensor detected")
    }
}

.checkCP <- function(num_components, darr){
    if(is.null(num_components)){
        stop("num_components must be specified")
    }
    if(.is_zero_tensor(darr)){
        stop("Zero tensor detected")
    }
}

.checkTUCKER <- function(ranks, darr){
    if(is.null(ranks)){
        stop("ranks must be specified")
    }
    if(sum(ranks > dim(darr)) != 0){
        stop("ranks must be smaller than the corresponding mode")
    }
    if(sum(ranks <= 0) != 0){
        stop("ranks must be positive")
    }
    if(.is_zero_tensor(darr)){
        stop("Zero tensor detected")
    }
}

.checkMPCA <- function(ranks, darr){
    if(is.null(ranks)){
        stop("ranks must be specified")
    }
    if(sum(ranks > dim(darr)[seq(2)]) != 0){
        stop("ranks must be smaller than the corresponding mode")
    }
    if(sum(ranks <= 0) != 0){
        stop("ranks must be positive")
    }
    if(.is_zero_tensor(darr)){
        stop("Zero tensor detected")
    }
}

.checkPVD <- function(uranks, wranks, a, b, darr){
    modes <- dim(darr)
    if(.ndim(darr) != 3){
        stop("PVD only for 3D")
    }
    if (sum(uranks <= 0) != 0){
        stop("uranks must be positive")
    }
    if (sum(wranks <= 0) != 0){
        stop("wranks must be positive")
    }
    if (.is_zero_tensor(darr)){
        stop("Zero tensor detected")
    }
    if(is.null(uranks) || is.null(wranks)){
        stop("U and V ranks must be specified")
    }
    if(is.null(a) || is.null(b)){
        stop("a and b must be specified")
    }
    n <- modes[3]
    if(length(uranks) != n || length(wranks) != n){
        stop("ranks must be of length n3")
    }
}

.checkModeBindList <- function(L, m){
    stopifnot(is.list(L))
    lapply(L, .checkDelayedArray)
    .checkCommonDims(L, m)
    .checkNumModes(L)
    if(is.null(m)){
        stop("Specify m (e.g. m=2)")
    }
}

.checkModeSum <- function(darr, m){
    if(is.null(m)){
        stop("must specify mode m")
    }
    num_modes <- .ndim(darr)
    if(m<1 || m>num_modes){
        stop("m out of bounds")
    }
}

.checkHadamard <- function(darr1, darr2){
    mode1s <- as.integer(dim(darr1))
    mode2s <- as.integer(dim(darr2))
    if (!identical(mode1s, mode2s)){
        msg <- paste0("darr1 and darr2 must have the same number ",
            "of rows and columns.")
        stop(msg)
    }
}

.checkKronecker <- function(darr1, darr2){
    if(.ndim(darr1) != .ndim(darr2)){
        msg <- paste0("Please confirm that the two DelayedArray has",
            " the same number of modes, otherwise we cannot recognize",
            " which modes corespond each other")
        stop(msg)
    }
}

.checkKhatri_Rao <- function(darr1, darr2){
    .check2D(darr1)
    .check2D(darr2)
    mode1s <- as.integer(dim(darr1))
    mode2s <- as.integer(dim(darr2))
    if (mode1s[2] != mode2s[2]){
        stop("Arguments must have same number of columns.")
    }
}

.checkUnfold <- function(darr, row_idx, col_idx){
    if(is.null(row_idx) || is.null(col_idx)){
        stop("row and column indices must be specified")
    }
    num_modes <- .ndim(darr)
    if(any(row_idx < 1) || any(row_idx > num_modes) ||
        any(col_idx < 1) || any(col_idx > num_modes)){
        stop("illegal indices specified")
    }
    perm <- c(row_idx, col_idx)
    if(any(sort(perm, decreasing=TRUE) != rev(seq_len(num_modes)))){
        stop("missing and/or repeated indices")
    }
}

.checkFold <- function(mat, row_idx, col_idx, modes){
    if(is.null(row_idx) || is.null(col_idx)){
        stop("row space and col space indices must be specified")
    }
    if(is.null(modes)){
        stop("DelayedTensor modes must be specified")
    }
    num_modes <- length(modes)
    stopifnot(num_modes == length(row_idx)+length(col_idx))
    mat_modes <- dim(mat)
    check1 <- mat_modes[1] != prod(modes[row_idx])
    check2 <- mat_modes[2] != prod(modes[col_idx])
    if(check1 || check2){
        stop("matrix nrow/ncol does not match Tensor modes")
    }
}

.checkCommonDims <- function(L, m){
    dim_ms <- unlist(lapply(L, function(x,m){dim(x)[m]}, m=m))
    if(!all(dim_ms == dim_ms[1])){
        msg <- paste0("m must be specified as the common dimensions ",
            "among all the arrays in the list")
        stop(msg)
    }
}

.checkNumModes <- function(L){
    num_modes <- unlist(lapply(L, function(x){.ndim(x)}))
    if(!all(num_modes == num_modes[1])){
        msg <- paste0("all the arrays in the list must have the same number ",
            "of modes (length(dim(x)))")
        stop(msg)
    }
}

.checkLimit <- function(x, block.size){
    .checkArrayGrid(x)
    blocks <- unlist(gridApply(x, function(x){prod(dim(x))}))
    if(!all(blocks <= block.size)){
        stop("The block size is too large!!!")
    }
}

"%ni%" <- Negate("%in%")

.checkDelayedArray <- function(x){
    if("DelayedArray" %ni% is(x)){
        stop("Please specify the input as DelayedArray")
    }
}

.checkArrayGrid <- function(x){
    if("ArrayGrid" %ni% is(x)){
        stop("Please specify the input as ArrayGrid")
    }
}

.check2D <- function(x){
    if(.ndim(x) != 2){
        stop("Only 2D array (matrix) can be specified")
    }
}

.realize_and_return <- function(x){
    backend <- getBackend()
    if(backend == "TileDBArray"){
        .writeTileDB_safe(x)
    }else{
        realize(x, "HDF5Array")
    }
}

# Block-by-block copy to TileDB, avoiding full-memory load.
# Workaround for TileDB-R [<- bug (TileDB-Inc/TileDB-R#877):
# write_block on TileDBRealizationSink fails for 2D ncol=1 (silent NaN)
# and 2D ncol=3 (misinterpreted as sparse triplets).
# 3D+ and 2D ncol=2,4+ work fine with block copy.
.writeTileDB_safe <- function(x){
    orig_dim <- dim(x)
    ndim <- length(orig_dim)
    # Determine if padding is needed
    needs_pad <- FALSE
    if(ndim == 1L){
        # 1D: reshape to 2D ncol=2 with dummy column
        x <- DelayedArray(array(c(as.vector(x), rep(0, orig_dim[1])),
            dim=c(orig_dim[1], 2L)))
        needs_pad <- TRUE
    }else if(ndim == 2L && orig_dim[2] %in% c(1L, 3L)){
        # Pad to ncol+1 via lazy cbind
        pad <- DelayedArray(matrix(0, nrow=orig_dim[1], ncol=1L))
        x <- arbind(t(x), t(pad))
        x <- t(x)
        needs_pad <- TRUE
    }
    # Block-by-block copy to TileDB sink
    out <- .blockCopyToTileDB(x)
    if(needs_pad){
        if(ndim == 1L){
            out <- out[, 1L, drop=TRUE]
            dim(out) <- orig_dim
            out <- DelayedArray(out)
        }else{
            out <- out[, seq_len(orig_dim[2]), drop=FALSE]
        }
    }
    # Normalize list(NULL,...) dimnames to NULL
    dn <- dimnames(out)
    if(!is.null(dn) && all(vapply(dn, is.null, logical(1)))){
        dimnames(out) <- NULL
    }
    out
}

# Copy a DelayedArray to TileDB block by block (never loads full array)
.blockCopyToTileDB <- function(darr){
    block.size <- getAutoBlockSize()
    modes <- dim(darr)
    spacings <- .blockSizeScheduling(modes, block.size)
    grid <- RegularArrayGrid(refdim=modes, spacings=spacings)
    sink <- TileDBArray::TileDBRealizationSink(modes)
    for(bid in seq_along(grid)){
        viewport <- grid[[bid]]
        block <- read_block(darr, viewport)
        sink <- write_block(sink, viewport, block)
        if(options()$delayedtensor.verbose){
            cat(paste0("\\ Copying block ",
                bid, "/", length(grid), " to TileDB ... OK\n"))
        }
    }
    close(sink)
    as(sink, "DelayedArray")
}

.create_sink <- function(dim){
    dim <- as.integer(dim)
    # Always use HDF5 sink for block processing.
    # TileDB's [<- is broken for ncol=1 (silent data loss) and ncol=3
    # (misinterpreted as sparse triplets). See TileDB-Inc/TileDB-R#877.
    # Conversion to TileDB happens in .sink_to_delayed() after close().
    setAutoRealizationBackend("HDF5Array")
    AutoRealizationSink(dim)
}


.sink_to_delayed <- function(sink){
    out <- as(sink, "DelayedArray")
    if(getBackend() == "TileDBArray"){
        # Block processing used HDF5 sink; convert to TileDB now
        out <- .realize_and_return(out)
    }
    out
}
