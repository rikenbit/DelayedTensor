#
# Utility function to merge multiple Tensor based on
# Einstein's summation convention
#
# subscripts = 'iiii->i'
# operands = list(darrG)
einsum <- function(subscripts, ...){
    # Preprocessing
    subscripts <- gsub(" ", "", subscripts)
    operands <- list(...)
    # Argument Check & Setting
    em <- .checkEinsum(subscripts, operands)
    all_modes <- em$all_modes
    modes <- em$modes
    new_modes <- em$new_modes
    check_perm <- em$check_perm
    block.size <- getAutoBlockSize()
    if(check_perm){
        .sortOperands(operands, new_modes, modes)
    }else{
        # Spacing
        all_spacings <- .blockSizeSchedulingList(all_modes, modes, block.size)
        ## operands & operands_grids
        operands_spacings <- .operands_spacings(modes, all_spacings)
        operands_grids <- .RegularArrayGridList(modes, operands_spacings)
        ## index
        idx_grids <- RegularArrayGrid(all_modes, all_spacings)
        ## sink
        sink_grid <- .RegularArrayGrid(new_modes, all_spacings)
        ## check
        lapply(operands_grids, function(x, bs){
            .checkLimit(x, bs)}, bs=block.size)
        .checkLimit(idx_grids, block.size)
        .checkLimit(sink_grid, block.size)
        # Block processing
        setAutoRealizationBackend("HDF5Array")
        sink <- AutoRealizationSink(as.vector(new_modes))
        for(bid in seq_along(idx_grids)){
            idx <- Lindex2Mindex(bid, dim(idx_grids))
            colnames(idx) <- names(dim(idx_grids))
            block1 <- .read_block(new_modes, idx, sink_grid, sink)
            block2 <- .block_einsum(subscripts, operands, operands_grids,
                modes, idx)
            sink <- .write_block(new_modes, idx, sink_grid,
                sink, block1, block2)
            cat(paste0("\\ Processing viewport ",
                bid, "/", length(idx_grids),
                " ... OK\n"))
        }
        close(sink)
        as(sink, "DelayedArray")
    }
}

# for Block processing
.block_einsum <- function(subscripts, operands, operands_grids, modes, idx){
    cmd1 <- lapply(seq_along(operands), function(x){
        paste0("idx", letters[x], " <- Mindex2Lindex(",
        "idx[, names(modes[[", x, "]])]",
        ", dim(operands_grids[[", x, "]]))")
    })
    cmd2 <- lapply(seq_along(operands), function(x){
        paste0(letters[x], " <- read_block(operands[[", x, "L]]",
            ", operands_grids[[", x, "L]][[idx", letters[x], "]])")
    })
    cmd3 <- paste0("einsum::einsum(subscripts, ",
        paste(letters[seq_along(operands)], collapse=", "), ")")
    eval(parse(text=cmd1))
    eval(parse(text=cmd2))
    eval(parse(text=cmd3))
}

# for sink
.tickMarksScheduling3 <- function(new_modes, all_spacings){
    if(identical(new_modes, 1L)){
        list(new_modes)
    }else{
        lapply(seq_along(new_modes), function(x){
            out <- as.vector(seq(1, new_modes[x], by=all_spacings[x]))
            if(identical(out, 1)){
                as.integer(new_modes[x])
            }else{
                as.integer(out)
            }
        })
    }
}

# for sink
.RegularArrayGrid <- function(new_modes, all_spacings){
    if(identical(new_modes, 1L)){
        RegularArrayGrid(1L, 1L)
    }else{
        RegularArrayGrid(as.vector(new_modes),
                all_spacings[names(new_modes)])
    }
}

# for sink
.read_block <- function(new_modes, idx, sink_grid, sink){
    if(identical(new_modes, 1L)){
        sink_idx <- 1L
    }else{
        midx <- idx[, names(new_modes)]
        sink_idx <- Mindex2Lindex(midx, dim(sink_grid))
    }
    read_block(as(sink, "DelayedArray"), sink_grid[[sink_idx]])
}

# for sink
.write_block <- function(new_modes, idx, sink_grid, sink, block1, block2){
    if(identical(new_modes, 1L)){
        sink_idx <- 1L
    }else{
        midx <- idx[, names(new_modes)]
        sink_idx <- Mindex2Lindex(midx, dim(sink_grid))
    }
    write_block(sink, sink_grid[[sink_idx]], block1 + block2)
}

# Return List
.blockSizeSchedulingList <- function(all_modes, modes, block.size){
    num_modes <- length(all_modes)
    all_spacings <- all_modes
    all_spacings[] <- 0
    for(i in seq_len(num_modes)){
        limit1 <- prod(all_modes[seq_len(i)])
        limit2 <- max(unlist(lapply(modes, function(x){
                    target <- unlist(lapply(names(x), function(xx){
                        which(names(xx) == names(all_modes[seq_len(i)]))
                    }))
                    prod(x[target])})))
        limit <- max(limit1, limit2)
        if(limit <= block.size){
            all_spacings[i] <- all_modes[i]
        }else{
            all_spacings[i] <- 1
        }
    }
    all_spacings
}

.operands_spacings <- function(modes, all_spacings){
    lapply(modes, function(m){as.vector(all_spacings[names(m)])})
}

.idx_spacings <- function(all_modes, all_spacings){
    out <- lapply(names(all_modes), function(m){
        as.vector(all_spacings[m])
    })
    names(out) <- names(all_modes)
    out
}

# Return List
.RegularArrayGridList <- function(modes, operands_spacings){
    lapply(seq_along(modes), function(x){
        RegularArrayGrid(refdim=as.vector(modes[[x]]),
            spacings=operands_spacings[[x]])
    })
}

# Argument Check & Setting
.checkEinsum <- function(subscripts, operands){
    no_operands <- length(operands)
    no_comma <- .no_comma(subscripts)
    if(no_comma == -1){
        no_comma <- 0
    }
    # DelayedArray
    lapply(operands, .checkDelayedArray)
    # X,X
    if(no_operands-1 != no_comma){
        msg <- paste0("(No. of operands - 1) and (No. of ,) are ",
            no_operands-1, " and ", no_comma, " and different!")
        stop(msg)
    }
    # ->
    if(length(grep("->", subscripts)) != 1){
        if(length(grep("->", subscripts)) == 0){
            msg <- paste0("Please make sure that equation_string ",
                "has explicit indicator (->)")
        }
        if(length(grep("->", subscripts)) > 1){
            msg <- paste0("Please make sure that equation_string ",
                "has single explicit indicator (->)")
        }
        stop(msg)
    }
    # Left part of ->
    lfs <- .left_sub(subscripts)
    split_subscripts <- strsplit(lfs, ",")[[1]]
    # Right part of ->
    rhs <- .right_sub(subscripts)
    #
    check_perm <- .check_perm(lfs, rhs)
    # all_modes
    all_modes <- .s_to_d(subscripts, operands)
    # modes (operands)
    names_modes <- lapply(seq_along(split_subscripts), function(x){
        strsplit(split_subscripts[x], "")[[1]]
    })
    modes <- lapply(seq_along(operands), function(x){
        tmp <- dim(operands[[x]])
        names(tmp) <- names_modes[[x]]
        tmp
    })
    names(modes) <- paste0("darr", seq_along(split_subscripts))
    # new_modes (for sink)
    if(is.na(rhs)){
        new_modes <- 1L
    }else{
        new_modes <- all_modes[strsplit(rhs, "")[[1]]]
    }
    # Output
    list(all_modes=all_modes, modes=modes, new_modes=new_modes,
        check_perm=check_perm)
}

# Sort (e.g. ji)
.sortOperands <- function(operands, new_modes, modes){
    orderO <- unlist(lapply(names(new_modes), function(x){
        which(x == names(modes[[1]]))
    }))
    .realize_and_return(aperm(operands[[1]], orderO))
}

# whether left and right are same
.check_perm <- function(lfs, rhs){
    left <- strsplit(lfs, "")[[1]]
    right <- strsplit(rhs, "")[[1]]
    identical(sort(left), sort(right))
}

# No. of comma
.no_comma <- function(subscripts){
    tmp = strsplit(subscripts, "")[[1]]
    length(grep(",", tmp))
}

# subscripts -> dims
.s_to_d <- function(subscripts, operands){
    odims <- unlist(lapply(operands, function(o){dim(o)}))
    sbs <- strsplit(gsub(",", "", .left_sub(subscripts)), "")[[1]]
    common.sbs <- sort(unique(sbs))
    out <- unlist(lapply(common.sbs, function(c){
        odims[which(sbs == c)[1]]
    }))
    names(out) <- common.sbs
    out
}

# left subscripts
.left_sub <- function(subscripts){
    strsplit(subscripts, "->")[[1]][1]
}

# right subscripts
.right_sub <- function(subscripts){
    strsplit(subscripts, "->")[[1]][2]
}