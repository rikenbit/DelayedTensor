#
# Tensor decomposition algorithms
#

# HOSVD (Tucker)
setGeneric("hosvd",
    function(darr, ranks=NULL){
        standardGeneric("hosvd")})
setMethod("hosvd", signature(darr="DelayedArray"),
    function(darr, ranks=NULL){
        .hosvd(darr, ranks)})
.hosvd <- function(darr, ranks){
    # Argument check
    .checkHOSVD(ranks, darr)
    # Setting
    num_modes <- .ndim(darr)
    #no truncation if ranks not provided
    if(is.null(ranks)){
        ranks <- dim(darr)
    }else{
        if (sum(ranks > dim(darr)) != 0){
            stop("ranks must be smaller than the corresponding mode")
        }
    }
    #progress bar
    pb <- txtProgressBar(min=0, max=num_modes, style=3)
    #loops through and performs SVD on mode-m matricization of darr
    U_list <- vector("list", num_modes)
    for(m in seq_len(num_modes)){
        temp_mat <- rs_unfold(darr, m=m)
        U_list[[m]] <- DelayedArray(.svd(temp_mat, k=ranks[m])$u)
        setTxtProgressBar(pb, m)
    }
    close(pb)
    #computes the core tensor
    Z <- ttl(darr, lapply(U_list, t), ms=seq_len(num_modes))
    est <- ttl(Z, U_list, ms=seq_len(num_modes))
    resid <- fnorm(est - darr)
    #put together the return list, and returns
    list(Z=Z, U=U_list, est=est, fnorm_resid=resid)
}

# CP
setGeneric("cp",
    function(darr, num_components=NULL, max_iter=25, tol=1e-5){
        standardGeneric("cp")})
setMethod("cp", signature(darr="DelayedArray"),
    function(darr, num_components=NULL, max_iter=25, tol=1e-5){
        .cp(darr, num_components, max_iter, tol)})
.cp <- function(darr, num_components, max_iter, tol){
    # Argument check
    .checkCP(num_components, darr)
    # Setting
    num_modes <- .ndim(darr)
    modes <- dim(darr)
    U_list <- unfolded_mat <- vector("list", num_modes)
    darr_norm <- fnorm(darr)
    for(m in seq_len(num_modes)){
        unfolded_mat[[m]] <- rs_unfold(darr, m=m)
        U_list[[m]] <- RandomNormArray(dim=c(modes[m], num_components))
    }
    est <- darr
    curr_iter <- 1
    converged <- FALSE
    fnorm_resid <- rep(0, max_iter)
    pb <- txtProgressBar(min=0, max=max_iter, style=3)
    while((curr_iter < max_iter) && (!converged)){
        setTxtProgressBar(pb, curr_iter)
        for(m in seq_len(num_modes)){
            V_inv <- solve(hadamard_list(lapply(U_list[-m], function(x){
                    t(x) %*% x})))
            tmp <- as(khatri_rao_list(U_list[-m],reverse=TRUE), "DelayedMatrix")
            tmp2 <- unfolded_mat[[m]] %*% tmp %*% V_inv
            lambdas <- colSums(abs(tmp2))
            U_list[[m]] <- sweep(tmp2, 2, lambdas, "/")
            Z <- DelayedDiagonalArray(
                rep(num_components, length=num_modes), lambdas)
            est <- ttl(Z, U_list, ms=seq_len(num_modes))
        }
        conv_cp <- .CHECK_CONV_CP(est, darr, curr_iter, fnorm_resid,
            darr_norm, tol)
        fnorm_resid <- conv_cp$fnorm_resid
        conv <- conv_cp$conv
        if(conv){
            converged <- TRUE
            setTxtProgressBar(pb, max_iter)
        }else{
            curr_iter <- curr_iter + 1
        }
    }
    if(!converged){setTxtProgressBar(pb, max_iter)}
    close(pb)
    fnorm_resid <- fnorm_resid[fnorm_resid != 0]
    norm_percent <- (1 - (tail(fnorm_resid, 1) / darr_norm)) * 100
    list(lambdas=lambdas, U=U_list, conv=converged, est=est,
        norm_percent=norm_percent, fnorm_resid = tail(fnorm_resid,1),
        all_resids=fnorm_resid)
}

.CHECK_CONV_CP <- function(est, darr, curr_iter, fnorm_resid, darr_norm, tol){
    curr_resid <- fnorm(est - darr)
    fnorm_resid[curr_iter] <- curr_resid
    if (curr_iter == 1){
        return(list(fnorm_resid=fnorm_resid, conv=FALSE))
    }else{
        if(abs(curr_resid-fnorm_resid[curr_iter-1])/darr_norm < tol){
            return(list(fnorm_resid=fnorm_resid, conv=TRUE))
        }
        else{
            return(list(fnorm_resid=fnorm_resid, conv=FALSE))
        }
    }
}

# HOOI (Tucker)
setGeneric("tucker",
    function(darr, ranks=NULL, max_iter=25, tol=1e-5){
        standardGeneric("tucker")})
setMethod("tucker", signature(darr="DelayedArray"),
    function(darr, ranks=NULL, max_iter=25, tol=1e-5){
        .tucker(darr, ranks, max_iter, tol)})
.tucker <- function(darr, ranks, max_iter, tol){
    # Argument check
    .checkTUCKER(ranks, darr)
    # Setting
    num_modes <- .ndim(darr)
    U_list <- vector("list", num_modes)
    for(m in seq_len(num_modes)){
        temp_mat <- rs_unfold(darr, m=m)
        U_list[[m]] <- DelayedArray(.svd(temp_mat, k=ranks[m])$u)
    }
    darr_norm <- fnorm(darr)
    curr_iter <- 1
    converged <- FALSE
    #set up convergence check
    fnorm_resid <- rep(0, max_iter)
    pb <- txtProgressBar(min=0, max=max_iter, style=3)
    #main loop (until convergence or max_iter)
    while((curr_iter < max_iter) && (!converged)){
        setTxtProgressBar(pb, curr_iter)
        modes <- .ndim(darr)
        modes_seq <- seq_len(num_modes)
        for(m in modes_seq){
            X <- ttl(darr, lapply(U_list[-m], t), ms=modes_seq[-m])
            U_list[[m]] <-
                DelayedArray(.svd(rs_unfold(X, m=m), k=ranks[m])$u)
        }
        #compute core tensor Z
        Z <- ttm(X, mat=t(U_list[[num_modes]]), m=num_modes)
        #checks convergence
        conv_tucker <- .CHECK_CONV_Tucker(Z, U_list, num_modes, est, darr,
            curr_iter, fnorm_resid, darr_norm, tol)
        fnorm_resid <- conv_tucker$fnorm_resid
        conv <- conv_tucker$conv
        if(conv){
            converged <- TRUE
            setTxtProgressBar(pb, max_iter)
        }else{
            curr_iter <- curr_iter + 1
        }
    }
    close(pb)
    fnorm_resid <- fnorm_resid[fnorm_resid != 0]
    norm_percent <- (1 - (tail(fnorm_resid, 1) / darr_norm)) * 100
    est <- ttl(Z, U_list, ms=seq_len(num_modes))
    list(Z=Z, U=U_list, conv=converged, est=est, norm_percent = norm_percent,
        fnorm_resid=tail(fnorm_resid, 1), all_resids=fnorm_resid)
}

.CHECK_CONV_Tucker <- function(Z, U_list, num_modes, est, darr,
    curr_iter, fnorm_resid, darr_norm, tol){
    est <- ttl(Z, U_list, ms=seq_len(num_modes))
    curr_resid <- fnorm(darr - est)
    fnorm_resid[curr_iter] <- curr_resid
    if(curr_iter == 1){
        return(list(fnorm_resid=fnorm_resid, conv=FALSE))
    }
    if(abs(curr_resid - fnorm_resid[curr_iter-1])/darr_norm < tol){
        return(list(fnorm_resid=fnorm_resid, conv=TRUE))
    }else{
        return(list(fnorm_resid=fnorm_resid, conv=FALSE))
    }
}

# MPCA
setGeneric("mpca",
    function(darr, ranks=NULL, max_iter=25, tol=1e-5){
        standardGeneric("mpca")})
setMethod("mpca", signature(darr="DelayedArray"),
    function(darr, ranks=NULL, max_iter=25, tol=1e-5){
        .mpca(darr, ranks, max_iter, tol)})
.mpca <- function(darr, ranks, max_iter, tol){
    # Argument check
    .checkMPCA(ranks, darr)
    # Setting
    num_modes <- .ndim(darr)
    stopifnot(length(ranks) == (num_modes - 1))
    ranks <- c(ranks, 1)
    modes <- dim(darr)
    U_list <- vector("list", num_modes)
    unfolded_mat <- vector("list", num_modes)
    for(m in seq_len(num_modes-1)){
        unfolded_mat <- rs_unfold(darr, m=m)
        mode_m_cov <- unfolded_mat %*% t(unfolded_mat)
        U_list[[m]] <- DelayedArray(.svd(mode_m_cov, k=ranks[m])$u)
    }
    Z_ext <- ttl(darr, lapply(U_list[-num_modes], t), ms=seq_len(num_modes-1))
    darr_norm <- fnorm(darr)
    curr_iter <- 1
    converged <- FALSE
    fnorm_resid <- rep(0, max_iter)
    pb <- txtProgressBar(min=0, max=max_iter, style=3)
    while((curr_iter < max_iter) && (!converged)){
        setTxtProgressBar(pb, curr_iter)
        modes <-dim(darr)
        modes_seq <- seq_len(num_modes-1)
        for(m in modes_seq){
            X <- ttl(darr, lapply(U_list[-c(m,num_modes)], t), ms=modes_seq[-m])
            U_list[[m]] <- DelayedArray(.svd(rs_unfold(X,m=m), k=ranks[m])$u)
        }
        Z_ext <- ttm(X, mat=t(U_list[[num_modes-1]]), m=num_modes-1)
        conv_mpca <- .CHECK_CONV_MPCA(Z_ext, U_list, num_modes, est, darr,
            curr_iter, fnorm_resid, darr_norm, tol)
        fnorm_resid <- conv_mpca$fnorm_resid
        conv <- conv_mpca$conv
        if(conv){
            converged <- TRUE
            setTxtProgressBar(pb, max_iter)
        }else{
            curr_iter <- curr_iter + 1
        }
    }
    close(pb)
    est <- ttl(Z_ext, U_list[-num_modes], ms=seq_len(num_modes-1))
    fnorm_resid <- fnorm_resid[fnorm_resid!=0]
    norm_percent <-(1 - (tail(fnorm_resid, 1) / darr_norm)) * 100
    list(Z_ext=Z_ext, U=U_list, conv=converged, est=est,
        norm_percent = norm_percent, fnorm_resid=tail(fnorm_resid,1),
        all_resids=fnorm_resid)
}

.CHECK_CONV_MPCA <- function(Z_ext, U_list, num_modes, est, darr, curr_iter,
fnorm_resid, darr_norm, tol){
    est <- ttl(Z_ext, U_list[-num_modes], ms=seq_len(num_modes-1))
    curr_resid <- fnorm(darr - est)
    fnorm_resid[curr_iter] <- curr_resid
    if(curr_iter == 1){
        return(list(fnorm_resid=fnorm_resid, conv=FALSE))
    }
    if(abs(curr_resid-fnorm_resid[curr_iter-1])/darr_norm < tol){
        return(list(fnorm_resid=fnorm_resid, conv=TRUE))
    }else{
        return(list(fnorm_resid=fnorm_resid, conv=FALSE))
    }
}

# PVD
setGeneric("pvd",
    function(darr, uranks=NULL, wranks=NULL, a=NULL, b=NULL){
        standardGeneric("pvd")})
setMethod("pvd", signature(darr="DelayedArray"),
    function(darr, uranks=NULL, wranks=NULL, a=NULL, b=NULL){
        .pvd(darr, uranks, wranks, a, b)})
.pvd <- function(darr, uranks, wranks, a, b){
    # Argument check
    .checkPVD(uranks, wranks, a, b, darr)
    # Setting
    uranks <- as.integer(uranks)
    wranks <- as.integer(wranks)
    a <- as.integer(a)
    b <- as.integer(b)
    modes <- dim(darr)
    n <- modes[3]
    pb <- txtProgressBar(min=0, max=(n+3), style=3)
    Us <- vector('list', n)
    Vs <- vector('list', n)
    S <- vector('list', n)
    for(i in seq_len(n)){
        svdz <- .svd(darr[,,i], k=max(uranks[i], wranks[i]))
        Us[[i]] <- DelayedArray(svdz$u[, seq_len(uranks[i])])
        Vs[[i]] <- DelayedArray(svdz$v[, seq_len(wranks[i])])
        S[[i]] <- svdz$d[seq_len(min(uranks[i], wranks[i]))]
        setTxtProgressBar(pb, i)
    }
    U <- cbind_list(Us)
    U <- as(cbind_list(list_rep(U, sum(uranks)*n/ncol(U))), "DelayedMatrix")
    UtU <- as(U %*% t(U), "DelayedMatrix")
    P <- DelayedArray(.svd(UtU, k=a)$u)
    setTxtProgressBar(pb, n+1)
    V <- cbind_list(Vs)
    V <- as(cbind_list(list_rep(V, sum(wranks)*n/ncol(V))), "DelayedMatrix")
    Dt <- DelayedArray(.svd(V %*% t(V), k=b)$u)
    D <- t(Dt)
    setTxtProgressBar(pb, n+2)
    V2 <- vector('list', n)
    est <- array(0, dim=modes)
    for(i in seq_len(n)){
        diagS <- DelayedDiagonalArray(c(uranks[i], wranks[i]), S[[i]])
        diagS <- as(diagS, "DelayedMatrix")
        V2[[i]] <- .realize_and_return((t(P) %*% Us[[i]]) %*% diagS %*%
            (t(Vs[[i]]) %*% Dt))
        V2[[i]] <- as(V2[[i]], "DelayedMatrix")
        est[,,i] <- as.array(P %*% V2[[i]] %*% D)
    }
    est <- DelayedArray(est)
    fnorm_resid <- fnorm(est - darr)
    setTxtProgressBar(pb, n+3)
    norm_percent <- (1 - (fnorm_resid / fnorm(darr))) * 100
    list(P=P, D=D, V=V2, est=est,
        norm_percent=norm_percent, fnorm_resid=fnorm_resid)
}

.is_zero_tensor <- function(darr){
    size <- prod(dim(darr))
    if(size >= 10^3){
        nzero <- sum(darr[seq_len(10^3)] == 0)
    }else{
        nzero <- sum(darr == 0)
    }
    if(all(nzero) == size){
        TRUE
    }
    FALSE
}

# Ref
# https://github.com/rikenbit/rTensor/blob/master/R/rTensor_Decomp.R