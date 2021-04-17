#
# Functions that operateon Matrices and Arrays
#

hadamard_list <- function(L){
	isvecORmat <- function(x){is.matrix(x) || is.vector(x)}
	stopifnot(all(unlist(lapply(L,isvecORmat))))
	retmat <- L[[1]]
	for (i in 2:length(L)){
		retmat <- retmat*L[[i]]
	}
	retmat
}

kronecker_list <- function(L){
	isvecORmat <- function(x){is.matrix(x) || is.vector(x)}
	stopifnot(all(unlist(lapply(L,isvecORmat))))
	retmat <- L[[1]]
	for(i in 2:length(L)){
		retmat <- kronecker(retmat,L[[i]])
	}
	retmat
}

khatri_rao <- function(x,y){
	if (!(is.matrix(x)&&is.matrix(y))) stop("Arguments must be matrices.")
	if (dim(x)[2]!=dim(y)[2]) stop("Arguments must have same number of columns.")
	retmat <- matrix(0,nrow=dim(x)[1]*dim(y)[1],ncol=dim(x)[2])
	for (j in 1:ncol(retmat)) retmat[,j] <- kronecker(x[,j],y[,j])
	retmat
}

khatri_rao_list <- function(L,reverse=FALSE){
	stopifnot(all(unlist(lapply(L,is.matrix))))
	ncols <- unlist(lapply(L,ncol))
	stopifnot(length(unique(ncols))==1)
	ncols <- ncols[1]
	nrows <- unlist(lapply(L,nrow))
	retmat <- matrix(0,nrow=prod(nrows),ncol=ncols)
	if (reverse) L <- rev(L)
	for(j in 1:ncols){
			Lj <- lapply(L,function(x) x[,j])
			retmat[,j] <- kronecker_list(Lj)
	}
	retmat
}

ttm<-function(tnsr, mat, m=NULL){
	stopifnot(is.matrix(mat))
	if(is.null(m)) stop("m must be specified")
	mat_dims <- dim(mat)
	modes_in <- tnsr@modes
	stopifnot(modes_in[m]==mat_dims[2])
	modes_out <- modes_in
	modes_out[m] <- mat_dims[1]
	tnsr_m <- rs_unfold(tnsr,m=m)@data
	retarr_m <- mat%*%tnsr_m
	rs_fold(retarr_m,m=m,modes=modes_out)
}

ttl<-function(tnsr,list_mat,ms=NULL){
	if(is.null(ms)||!is.vector(ms)) stop ("m modes must be specified as a vector")
	if(length(ms)!=length(list_mat)) stop("m modes length does not match list_mat length")
	num_mats <- length(list_mat)
	if(length(unique(ms))!=num_mats) warning("consider pre-multiplying matrices for the same m for speed")
	mat_nrows <- vector("list", num_mats)
	mat_ncols <- vector("list", num_mats)
	for(i in 1:num_mats){
	mat <- list_mat[[i]]
	m <- ms[i]
	mat_dims <- dim(mat)
	modes_in <- tnsr@modes
	stopifnot(modes_in[m]==mat_dims[2])
	modes_out <- modes_in
	modes_out[m] <- mat_dims[1]
	tnsr_m <- rs_unfold(tnsr,m=m)@data
	retarr_m <- mat%*%tnsr_m
	tnsr <- rs_fold(retarr_m,m=m,modes=modes_out)
	}
	tnsr
}

t_mult <- function(x,y){
	if((x@num_modes>3)||(y@num_modes>3)) stop("Tensor Multiplication currently only implemented for 3-Tensors")
	modes_x <- x@modes
	modes_y <- y@modes
	if(modes_x[2]!=modes_y[1]) stop("Mode 2 of x and Mode 1 of y must match")
	n3 <- modes_x[3]
	if(n3!=modes_y[3]) stop("Modes 3 of x and y must match")
	#fft's for x and y
	fft_x <- aperm(apply(x@data,MARGIN=1:2,fft),c(2,3,1))
	fft_y <- aperm(apply(y@data,MARGIN=1:2,fft),c(2,3,1))
	#multiply the faces (this is terribad! TO-DO: think of better way!)
	fft_ret <- array(0,dim=c(modes_x[1],modes_y[2],n3))
	for(i in 1:n3){
		first <- fft_x[,,i,drop=FALSE]
		second <- fft_y[,,i,drop=FALSE]
		fft_ret[,,i]<-matrix(first,nrow=dim(first)[1])%*%matrix(second,,nrow=dim(second)[1])
	}
	#ifft and return as Tensor
	#.ifft <- function(x){suppressWarnings(as.numeric(fft(x,inverse=TRUE))/length(x))}
	as.delayed.tensor(aperm(apply(fft_ret,MARGIN=1:2, .ifft),c(2,3,1)),drop=FALSE)
}

rand_tensor <- function(modes=c(3,4,5),drop=FALSE){
	as.delayed.tensor(array(rnorm(prod(modes)), dim=modes),drop=drop)
}

fold <- function(mat, row_idx = NULL, col_idx = NULL, modes=NULL){
	#checks
	rs <- row_idx
	cs <- col_idx
	if(is.null(rs)||is.null(cs)) stop("row space and col space indices must be specified")
	if(is.null(modes)) stop("DelayedTensor modes must be specified")
	if(!is(mat,"DelayedTensor")){
		if(!is.matrix(mat))  stop("mat must be of class 'matrix'")
		}else{
			stopifnot(mat@num_modes==2)
			mat <- mat@data
			}
	num_modes <- length(modes)
	stopifnot(num_modes==length(rs)+length(cs))
	mat_modes <- dim(mat)
	if((mat_modes[1]!=prod(modes[rs])) || (mat_modes[2]!=prod(modes[cs]))) stop("matrix nrow/ncol does not match Tensor modes")
	#rearranges into array
	iperm <- match(1:num_modes,c(rs,cs))
	as.delayed.tensor(aperm(array(mat,dim=c(modes[rs],modes[cs])),iperm))
}

k_fold <- function(mat, m=NULL, modes=NULL){
	if(is.null(m)) stop("mode m must be specified")
	if(is.null(modes)) stop("DelayedTensor modes must be specified")
	num_modes <- length(modes)
	rs <- m
	cs <- (1:num_modes)[-m]
	fold(mat,row_idx=rs,col_idx=cs,modes=modes)
}

unmatvec <- function(mat, modes=NULL){
	if(is.null(modes)) stop("DelayedTensor modes must be specified")
	num_modes <- length(modes)
	cs <- 2
	rs <- (1:num_modes)[-2]
	fold(mat,row_idx=rs,col_idx=cs,modes=modes)
}

rs_fold <- function(mat, m=NULL, modes=NULL){
	if(is.null(m)) stop("mode m must be specified")
	if(is.null(modes)) stop("DelayedTensor modes must be specified")
	num_modes <- length(modes)
	rs <- m
	cs <- (1:num_modes)[-m]
	fold(mat,row_idx=rs,col_idx=cs,modes=modes)
}

cs_fold <- function(mat, m=NULL, modes=NULL){
	if(is.null(m)) stop("mode m must be specified")
	if(is.null(modes)) stop("DelayedTensor modes must be specified")
	num_modes <- length(modes)
	cs <- m
	rs <- (1:num_modes)[-m]
	fold(mat,row_idx=rs,col_idx=cs,modes=modes)
}


.ifft <- function(x){
	suppressWarnings(
		as.numeric(
			fft(x,inverse=TRUE))/length(x))}

.superdiagonal_tensor <- function(num_modes,len,elements=1L){
	modes <- rep(len,num_modes)
	arr <- array(0, dim = modes)
	if(length(elements)==1) elements <- rep(elements,len)
	for (i in 1:len){
		txt <- paste("arr[",paste(rep("i", num_modes),collapse=","),"] <- ", elements[i],sep="")
		eval(parse(text=txt))
	}
	as.delayed.tensor(arr)
}

# Ref
# https://github.com/cran/rTensor/blob/master/R/rTensor_Misc.R