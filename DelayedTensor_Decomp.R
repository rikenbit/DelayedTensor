hosvd <- function(tnsr,ranks=NULL){
	stopifnot(is(tnsr,"DelayedTensor"))
	if (sum(ranks<=0)!=0) stop("ranks must be positive")
	if (.is_zero_tensor(tnsr)) stop("Zero tensor detected")

	num_modes <- tnsr@num_modes
	#no truncation if ranks not provided
	if(is.null(ranks)){
		ranks <- tnsr@modes
	}else{
		if (sum(ranks>tnsr@modes)!=0) stop("ranks must be smaller than the corresponding mode")
	}
	#progress bar
	pb <- txtProgressBar(min=0,max=num_modes,style=3)
	#loops through and performs SVD on mode-m matricization of tnsr
	U_list <- vector("list",num_modes)
	for(m in 1:num_modes){
		temp_mat <- rs_unfold(tnsr,m=m)@data
		U_list[[m]] <- svd(temp_mat,nu=ranks[m])$u
		setTxtProgressBar(pb,m)
	}
	close(pb)
	#computes the core tensor
	Z <- ttl(tnsr,lapply(U_list,t),ms=1:num_modes)
	est <- ttl(Z,U_list,ms=1:num_modes)
	resid <- fnorm(est-tnsr)
	#put together the return list, and returns
	list(Z=Z,U=U_list,est=est,fnorm_resid=resid)
}

cp <- function(tnsr, num_components=NULL,max_iter=25, tol=1e-5){
	if(is.null(num_components)) stop("num_components must be specified")
	stopifnot(is(tnsr,"DelayedTensor"))
	if (.is_zero_tensor(tnsr)) stop("Zero tensor detected")

	#initialization via truncated hosvd
	num_modes <- tnsr@num_modes
	modes <- tnsr@modes
	U_list <- vector("list",num_modes)
	unfolded_mat <- vector("list",num_modes)
	tnsr_norm <- fnorm(tnsr)
	for(m in 1:num_modes){
		unfolded_mat[[m]] <- rs_unfold(tnsr,m=m)@data
		U_list[[m]] <- matrix(rnorm(modes[m]*num_components), nrow=modes[m], ncol=num_components)
	}
	est <- tnsr
	curr_iter <- 1
	converged <- FALSE
	#set up convergence check
	fnorm_resid <- rep(0, max_iter)
	CHECK_CONV <- function(est){
		curr_resid <- fnorm(est - tnsr)
		fnorm_resid[curr_iter] <<- curr_resid
		if (curr_iter==1) return(FALSE)
		if (abs(curr_resid-fnorm_resid[curr_iter-1])/tnsr_norm < tol) return(TRUE)
		else{ return(FALSE)}
	}
	#progress bar
	pb <- txtProgressBar(min=0,max=max_iter,style=3)
	#main loop (until convergence or max_iter)
	norm_vec <- function(vec){
	norm(as.matrix(vec))
	}
	while((curr_iter < max_iter) && (!converged)){
	setTxtProgressBar(pb,curr_iter)
		for(m in 1:num_modes){
			V <- hadamard_list(lapply(U_list[-m],function(x) {t(x)%*%x}))
			V_inv <- solve(V)
			tmp <- unfolded_mat[[m]]%*%khatri_rao_list(U_list[-m],reverse=TRUE)%*%V_inv
			lambdas <- apply(tmp,2,norm_vec)
			U_list[[m]] <- sweep(tmp,2,lambdas,"/")
			Z <- .superdiagonal_tensor(num_modes=num_modes,len=num_components,elements=lambdas)
			est <- ttl(Z,U_list,ms=1:num_modes)
		}
		#checks convergence
		if(CHECK_CONV(est)){
			converged <- TRUE
			setTxtProgressBar(pb,max_iter)
		}else{
			curr_iter <- curr_iter + 1
			 }
	}
	if(!converged){setTxtProgressBar(pb,max_iter)}
	close(pb)
	#end of main loop
	#put together return list, and returns
	fnorm_resid <- fnorm_resid[fnorm_resid!=0]
	norm_percent<- (1-(tail(fnorm_resid,1)/tnsr_norm))*100
	invisible(list(lambdas=lambdas, U=U_list, conv=converged, est=est, norm_percent=norm_percent, fnorm_resid = tail(fnorm_resid,1),all_resids=fnorm_resid))
}

tucker <- function(tnsr,ranks=NULL,max_iter=25,tol=1e-5){
	stopifnot(is(tnsr,"DelayedTensor"))
	if(is.null(ranks)) stop("ranks must be specified")
	if (sum(ranks>tnsr@modes)!=0) stop("ranks must be smaller than the corresponding mode")
	if (sum(ranks<=0)!=0) stop("ranks must be positive")
	if (.is_zero_tensor(tnsr)) stop("Zero tensor detected")

	#initialization via truncated hosvd
	num_modes <- tnsr@num_modes
	U_list <- vector("list",num_modes)
	for(m in 1:num_modes){
		temp_mat <- rs_unfold(tnsr,m=m)@data
		U_list[[m]] <- svd(temp_mat,nu=ranks[m])$u
	}
	tnsr_norm <- fnorm(tnsr)
	curr_iter <- 1
	converged <- FALSE
	#set up convergence check
	fnorm_resid <- rep(0, max_iter)
	CHECK_CONV <- function(Z,U_list){
		est <- ttl(Z,U_list,ms=1:num_modes)
		curr_resid <- fnorm(tnsr - est)
		fnorm_resid[curr_iter] <<- curr_resid
		if (curr_iter==1) return(FALSE)
		if (abs(curr_resid-fnorm_resid[curr_iter-1])/tnsr_norm < tol) return(TRUE)
		else{return(FALSE)}
	}
	#progress bar
	pb <- txtProgressBar(min=0,max=max_iter,style=3)
	#main loop (until convergence or max_iter)
	while((curr_iter < max_iter) && (!converged)){
	setTxtProgressBar(pb,curr_iter)
	modes <- tnsr@modes
	modes_seq <- 1:num_modes
		for(m in modes_seq){
			#core Z minus mode m
			X <- ttl(tnsr,lapply(U_list[-m],t),ms=modes_seq[-m])
			#truncated SVD of X
			#U_list[[m]] <- (svd(rs_unfold(X,m=m)@data,nu=ranks[m],nv=prod(modes[-m]))$u)[,1:ranks[m]]
			U_list[[m]] <- svd(rs_unfold(X,m=m)@data,nu=ranks[m])$u
		}
		#compute core tensor Z
		Z <- ttm(X,mat=t(U_list[[num_modes]]),m=num_modes)

		#checks convergence
		if(CHECK_CONV(Z, U_list)){
			converged <- TRUE
			setTxtProgressBar(pb,max_iter)
		}else{
			curr_iter <- curr_iter + 1
			}
	}
	close(pb)
	#end of main loop
	#put together return list, and returns
	fnorm_resid <- fnorm_resid[fnorm_resid!=0]
	norm_percent<-(1-(tail(fnorm_resid,1)/tnsr_norm))*100
	est <- ttl(Z,U_list,ms=1:num_modes)
	invisible(list(Z=Z, U=U_list, conv=converged, est=est, norm_percent = norm_percent, fnorm_resid=tail(fnorm_resid,1), all_resids=fnorm_resid))
}

mpca <- function(tnsr, ranks = NULL, max_iter = 25, tol=1e-5){
	if(is.null(ranks)) stop("ranks must be specified")
	stopifnot(is(tnsr,"DelayedTensor"))
	if (sum(ranks>tnsr@modes)!=0) stop("ranks must be smaller than the corresponding mode")
	if (sum(ranks<=0)!=0) stop("ranks must be positive")
	if (.is_zero_tensor(tnsr)) stop("Zero tensor detected")

	#initialization via hosvd of M-1 modes
	num_modes <- tnsr@num_modes
	stopifnot(length(ranks)==(num_modes-1))
	ranks <- c(ranks,1)
	modes <- tnsr@modes
	U_list <- vector("list",num_modes)
	unfolded_mat <- vector("list",num_modes)
	for(m in 1:(num_modes-1)){
		unfolded_mat <- rs_unfold(tnsr,m=m)@data
		mode_m_cov <- unfolded_mat%*%t(unfolded_mat)
		U_list[[m]] <- svd(mode_m_cov, nu=ranks[m])$u
	}
	Z_ext <- ttl(tnsr,lapply(U_list[-num_modes],t),ms=1:(num_modes-1))
	tnsr_norm <- fnorm(tnsr)
	curr_iter <- 1
	converged <- FALSE
	#set up convergence check
	fnorm_resid <- rep(0, max_iter)
	CHECK_CONV <- function(Z_ext,U_list){
		est <- ttl(Z_ext,U_list[-num_modes],ms=1:(num_modes-1))
		curr_resid <- fnorm(tnsr - est)
		fnorm_resid[curr_iter] <<- curr_resid
		if (curr_iter==1) return(FALSE)
		if (abs(curr_resid-fnorm_resid[curr_iter-1])/tnsr_norm < tol) return(TRUE)
		else{return(FALSE)}
	}
	#progress bar
	pb <- txtProgressBar(min=0,max=max_iter,style=3)
	#main loop (until convergence or max_iter)
	while((curr_iter < max_iter) && (!converged)){
	setTxtProgressBar(pb,curr_iter)
	modes <-tnsr@modes
	modes_seq <- 1:(num_modes-1)
		for(m in modes_seq){
			#extended core Z minus mode m
			X <- ttl(tnsr,lapply(U_list[-c(m,num_modes)],t),ms=modes_seq[-m])
			#truncated SVD of X
			U_list[[m]] <- svd(rs_unfold(X,m=m)@data,nu=ranks[m])$u
		}
		#compute core tensor Z_ext
		Z_ext <- ttm(X,mat=t(U_list[[num_modes-1]]),m=num_modes-1)
		#checks convergence
		if(CHECK_CONV(Z_ext, U_list)){
			converged <- TRUE
			setTxtProgressBar(pb,max_iter)
		}else{
			curr_iter <- curr_iter + 1
			}
	}
	close(pb)
	#end of main loop
	#put together return list, and returns
	est <- ttl(Z_ext,U_list[-num_modes],ms=1:(num_modes-1))
	fnorm_resid <- fnorm_resid[fnorm_resid!=0]
	norm_percent<-(1-(tail(fnorm_resid,1)/tnsr_norm))*100
	invisible(list(Z_ext=Z_ext, U=U_list, conv=converged, est=est, norm_percent = norm_percent, fnorm_resid=tail(fnorm_resid,1), all_resids=fnorm_resid))
}

pvd <- function(tnsr,uranks=NULL,wranks=NULL,a=NULL,b=NULL){
	if(tnsr@num_modes!=3) stop("PVD only for 3D")
	if (sum(uranks<=0)!=0) stop("uranks must be positive")
	if (sum(wranks<=0)!=0) stop("wranks must be positive")
	if (.is_zero_tensor(tnsr)) stop("Zero tensor detected")

	if(is.null(uranks)||is.null(wranks)) stop("U and V ranks must be specified")
	if(is.null(a)||is.null(b)) stop("a and b must be specified")
	modes <- tnsr@modes
	n <- modes[3]
	if(length(uranks)!=n||length(wranks)!=n) stop("ranks must be of length n3")
	pb <- txtProgressBar(min=0,max=(n+3),style=3)
	x <- tnsr@data
	Us <- vector('list',n)
	Vs <- vector('list',n)
	S <- vector('list',n)
	for(i in 1:n){
		svdz <- svd(x[,,i],nu=uranks[i],nv=wranks[i])
		Us[[i]] <- svdz$u
		Vs[[i]] <- svdz$v
		S[[i]] <- svdz$d[1:min(uranks[i],wranks[i])]
		setTxtProgressBar(pb,i)
	}
	U <- matrix(unlist(Us),nrow=modes[1],ncol=sum(uranks)*n)
	#eigenU <- eigen(U%*%t(U))
	P <- eigen(U%*%t(U))$vectors[,1:a] #E-vecs of UU^T
	setTxtProgressBar(pb,n+1)
	V <- matrix(unlist(Vs),nrow=modes[2],ncol=sum(wranks)*n)
	#eigenV <- eigen(V%*%t(V))
	Dt <- eigen(V%*%t(V))$vectors[,1:b] #E-vecs of VV^T
	D <- t(Dt)
	setTxtProgressBar(pb,n+2)
	V2 <- vector('list',n)
	est <- array(0,dim=modes)
	for(i in 1:n){
		V2[[i]] <- (t(P)%*%Us[[i]])%*%diag(S[[i]],nrow=uranks[i],ncol=wranks[i])%*%(t(Vs[[i]])%*%Dt)
		est[,,i] <- P%*%V2[[i]]%*%D
	}
	est <- as.delayed.tensor(est)
	fnorm_resid <- fnorm(est-tnsr)
	setTxtProgressBar(pb,n+3)
	norm_percent<-(1-(fnorm_resid/fnorm(tnsr)))*100
	invisible(list(P=P,D=D,V=V2,est=est,norm_percent=norm_percent,fnorm_resid=fnorm_resid))
}

t_svd<-function(tnsr){
	if(tnsr@num_modes!=3) stop("T-SVD only implemented for 3d so far")
	if (.is_zero_tensor(tnsr)) stop("Zero tensor detected")

	modes <- tnsr@modes
	n1 <- modes[1]
	n2 <- modes[2]
	n3 <- modes[3]
	#progress bar
	pb <- txtProgressBar(min=0,max=n3,style=3)
	#define ifft
	#.ifft <- function(x){suppressWarnings(as.numeric(fft(x,inverse=TRUE))/length(x))}
	#fft for each of the n1n2 vectors (of length n3) along mode 3
	fftz <- aperm(apply(tnsr@data,MARGIN=1:2,fft),c(2,3,1))
	#svd for each face (svdz is a list of the results)
	U_arr <- array(0,dim=c(n1,n1,n3))
	V_arr <- array(0,dim=c(n2,n2,n3))
	m <- min(n1,n2)
	S_arr <- array(0,dim=c(n1,n2,n3))
	#Think of a way to avoid a loop in the beginning
	#Problem is that svd returns a list but ideally we want 3 arrays
	#Even with unlist this doesn't seem possible
	for (j in 1:n3){
		setTxtProgressBar(pb,j)
		decomp <- svd(fftz[,,j],nu=n1,nv=n2)
		U_arr[,,j] <- decomp$u
		V_arr[,,j] <- decomp$v
		S_arr[,,j] <- diag(decomp$d,nrow=n1,ncol=n2) #length is min(n1,n2)
	}
	close(pb)
	#for each svd result, we want to apply ifft
	U <- as.delayed.tensor(aperm(apply(U_arr,MARGIN=1:2, .ifft),c(2,3,1)))
	V <- as.delayed.tensor(aperm(apply(V_arr,MARGIN=1:2, .ifft),c(2,3,1)))
	S <- as.delayed.tensor(aperm(apply(S_arr,MARGIN=1:2, .ifft),c(2,3,1)))
	invisible(list(U=U,V=V,S=S))
}

t_svd_reconstruct <- function(L){
	t_mult(t_mult(L$U,L$S),t(L$V))
}

.is_zero_tensor <- function(tnsr){
	if (sum(tnsr@data==0)==prod(tnsr@modes)) return(TRUE)
	return(FALSE)
}

.t_compress <- function(tnsr,k){
	modes <- tnsr@modes
	n1 <- modes[1]
	n2 <- modes[2]
	n3 <- modes[3]
	#progress bar
	pb <- txtProgressBar(min=0,max=n3,style=3)
	#define ifft
	#.ifft <- function(x){suppressWarnings(as.numeric(fft(x,inverse=TRUE))/length(x))}
	#fft for each of the n1n2 vectors (of length n3) along mode 3
	fftz <- aperm(apply(tnsr@data,MARGIN=1:2,fft),c(2,3,1))
	#svd for each face (svdz is a list of the results)
	U_arr <- array(0,dim=c(n1,n1,n3))
	V_arr <- array(0,dim=c(n2,n2,n3))
	m <- min(n1,n2)
	S_arr <- array(0,dim=c(n1,n2,n3))
	#Think of a way to avoid a loop in the beginning
	#Problem is that svd returns a list but ideally we want 3 arrays
	#Even with unlist this doesn't seem possible
	for (j in 1:n3){
		setTxtProgressBar(pb,j)
		decomp <- svd(fftz[,,j],nu=n1,nv=n2)
		U_arr[,,j] <- decomp$u
		V_arr[,,j] <- decomp$v
		S_arr[,,j] <- diag(decomp$d,nrow=n1,ncol=n2) #length is min(n1,n2)
	}
	close(pb)
	#for each svd result, we want to apply ifft
	U <- as.delayed.tensor(aperm(apply(U_arr,MARGIN=1:2, .ifft),c(2,3,1)))
	V <- as.delayed.tensor(aperm(apply(V_arr,MARGIN=1:2, .ifft),c(2,3,1)))
	S <- as.delayed.tensor(aperm(apply(S_arr,MARGIN=1:2, .ifft),c(2,3,1)))

	est <- as.delayed.tensor(array(0,dim=modes))
	for (i in 1:k){
		est <- est + t_mult(t_mult(U[,i,,drop=FALSE],S[i,i,,drop=FALSE]),t(V[,i,,drop=FALSE]))
	}
	resid <- fnorm(est-tnsr)
	invisible(list(est=est, fnorm_resid = resid, norm_percent = (1-resid/fnorm(tnsr))*100))
}

.t_compress2 <- function(tnsr,k1,k2){
	A = modeSum(tnsr,m=3,drop=TRUE)
	svdz <- svd(A@data,nu=k1,nv=k2)
	Util <- svdz$u
	Vtil <- svdz$v
	modes <- tnsr@modes
	n3 <- modes[3]
	core <- array(0,dim=c(k1,k2,n3))
	for(i in 1:n3){
	core[,,i]<-t(Util)%*%tnsr[,,i]@data%*%Vtil
	}
	est <- array(0,dim=modes)
	for(i in 1:k1){
		for (j in 1:k2){
			est = est + Util[,i] %o% Vtil[,j] %o% core[i,j,]
		}
	}
	resid <- fnorm(tnsr - est)
	invisible(list(core = as.delayed.tensor(core), est=est, fnorm_resid = resid, norm_percent = (1-resid/fnorm(tnsr))*100))
}

# Ref
# https://github.com/cran/rTensor/blob/master/R/rTensor_Decomp.R