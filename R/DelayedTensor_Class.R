setGeneric(name="unfold",
def=function(tnsr,row_idx,col_idx){standardGeneric("unfold")})

setGeneric(name="k_unfold",
def=function(tnsr,m){standardGeneric("k_unfold")})

setGeneric(name="matvec",
           def=function(tnsr){standardGeneric("matvec")})

setGeneric(name="rs_unfold",
def=function(tnsr,m){standardGeneric("rs_unfold")})

setGeneric(name="cs_unfold",
def=function(tnsr,m){standardGeneric("cs_unfold")})

setGeneric(name="modeSum",
def=function(tnsr,m,drop){standardGeneric("modeSum")})

setGeneric(name="modeMean",
def=function(tnsr,m,drop){standardGeneric("modeMean")})

setGeneric(name="fnorm",
def=function(tnsr){standardGeneric("fnorm")})

setGeneric(name="innerProd",
def=function(tnsr1,tnsr2){standardGeneric("innerProd")})


###Method Definitions
options(warn=-1)

setMethod(f="dim",
signature="DelayedTensor",
definition=function(x){
	x@modes
})

setMethod(f="show",
signature="DelayedTensor",
definition=function(object){
	cat("Numeric Tensor of", object@num_modes, "Modes\n", sep=" ")
	cat("Modes: ", object@modes, "\n", sep=" ")
	cat("Data: \n")
	print(head(object@data))
})

setMethod(f="print",
signature="DelayedTensor",
definition=function(x,...){
	show(x)
})

setMethod(f="head",
signature="DelayedTensor",
definition=function(x,...){
	head(x@data,...)
})

setMethod(f="tail",
signature="DelayedTensor",
definition=function(x,...){
	tail(x@data,...)
})

setMethod("[", signature="DelayedTensor",
definition=function(x,i,j,...,drop=TRUE){
	if(!drop) as.delayed.tensor(`[`(x@data,i,j,drop=FALSE,...),drop=drop)
	else as.delayed.tensor(`[`(x@data,i,j,...))
})

setMethod("[<-", signature="DelayedTensor",
definition=function(x,i,j,...,value){
	if (is(value,"DelayedTensor")){
		as.delayed.tensor(`[<-`(x@data,i,j,...,value=value@data))
	}else{
		as.delayed.tensor(`[<-`(x@data,i,j,...,value=value))
	}
})

setMethod("t",signature="DelayedTensor",
definition=function(x){
	tnsr <- x
	if(tnsr@num_modes!=3) stop("Tensor Transpose currently only implemented for 3d Tensors")
	modes <- tnsr@modes
	new_arr <- array(apply(tnsr@data[,,c(1L,modes[3]:2L),drop=FALSE],MARGIN=3,FUN=t),dim=modes[c(2,1,3)])
	as.delayed.tensor(new_arr)
})

setMethod("modeSum",signature="DelayedTensor",
definition=function(tnsr,m=NULL,drop=FALSE){
	if(is.null(m)) stop("must specify mode m")
	num_modes <- tnsr@num_modes
	if(m<1||m>num_modes) stop("m out of bounds")
	perm <- c(m,(1L:num_modes)[-m])
	modes <- tnsr@modes
	newmodes <- modes; newmodes[m]<-1
	arr <- array(colSums(aperm(tnsr@data,perm),dims=1L),dim=newmodes)
	as.delayed.tensor(arr,drop=drop)
})

setMethod("modeMean",signature="DelayedTensor",
definition=function(tnsr,m=NULL,drop=FALSE){
	if(is.null(m)) stop("must specify mode m")
	num_modes <- tnsr@num_modes
	if(m<1||m>num_modes) stop("m out of bounds")
	perm <- c(m,(1L:num_modes)[-m])
	modes <- tnsr@modes
	newmodes <- modes; newmodes[m]<-1
	arr <- array(colSums(aperm(tnsr@data,perm),dims=1L),dim=newmodes)
	as.delayed.tensor(arr/modes[m],drop=drop)
})

setMethod("fnorm",signature="DelayedTensor",
definition=function(tnsr){
	arr<-tnsr@data
	sqrt(sum(arr*arr))
})

setMethod("innerProd",signature=c(tnsr1="DelayedTensor", tnsr2="DelayedTensor"),
definition=function(tnsr1,tnsr2){
	stopifnot(tnsr1@modes==tnsr2@modes)
	arr1 <- tnsr1@data
	arr2 <- tnsr2@data
	sum(as.numeric(arr1*arr2))
})

setMethod("unfold", signature="DelayedTensor",
definition=function(tnsr,row_idx=NULL,col_idx=NULL){
	#checks
	rs <- row_idx
	cs <- col_idx
	if(is.null(rs)||is.null(cs)) stop("row and column indices must be specified")
	num_modes <- tnsr@num_modes
	if (length(rs) + length(cs) != num_modes) stop("incorrect number of indices")
	if(any(rs<1) || any(rs>num_modes) || any(cs < 1) || any(cs>num_modes)) stop("illegal indices specified")
	perm <- c(rs,cs)
	if (any(sort(perm,decreasing=TRUE) != num_modes:1)) stop("missing and/or repeated indices")
	modes <- tnsr@modes
	mat <- tnsr@data
	new_modes <- c(prod(modes[rs]),prod(modes[cs]))
	#rearranges into a matrix
	mat <- aperm(mat,perm)
	dim(mat) <- new_modes
	as.delayed.tensor(mat)
})

setMethod("k_unfold", signature="DelayedTensor",
definition=function(tnsr,m=NULL){
	if(is.null(m)) stop("mode m must be specified")
	num_modes <- tnsr@num_modes
	rs <- m
	cs <- (1:num_modes)[-m]
	unfold(tnsr,row_idx=rs,col_idx=cs)
})

setMethod('matvec',signature="DelayedTensor",
          definition=function(tnsr){
          if(tnsr@num_modes!=3) stop("Matvec currently only implemented for 3d Tensors")
          num_modes <- tnsr@num_modes
          stopifnot(num_modes==3)
          unfold(tnsr,row_idx=c(1,3),col_idx=2)
          })

setMethod("rs_unfold", signature="DelayedTensor",
definition=function(tnsr,m=NULL){
	if(is.null(m)) stop("mode m must be specified")
	num_modes <- tnsr@num_modes
	rs <- m
	cs <- (1:num_modes)[-m]
	unfold(tnsr,row_idx=rs,col_idx=cs)
})

setMethod("cs_unfold", signature="DelayedTensor",
definition=function(tnsr,m=NULL){
	if(is.null(m)) stop("mode m must be specified")
	num_modes <- tnsr@num_modes
	rs <- (1:num_modes)[-m]
	cs <- m
	unfold(tnsr,row_idx=rs,col_idx=cs)
})

options(warn=1)

as.delayed.matrix <- function(x,drop=FALSE){
	stopifnot(is.array(x)||is.vector(x))
	if (is.vector(x)){
		modes <- c(length(x))
		num_modes <- 1L
		new("DelayedTensor", num_modes, modes, data = x)
	}
	else {
		modes <- dim(x)
		num_modes <- length(modes)
		dim1s <- which(modes==1)
		if (drop && (length(dim1s)>0)){
			modes <- modes[-dim1s]
			num_modes <- num_modes-length(dim1s)
			new("DelayedTensor",num_modes,modes,data=array(x,dim=modes))
		}
		else{
			new("DelayedTensor",num_modes,modes,data=x)
		}
	}
}

as.delayed.tensor <- function(x,drop=FALSE){
	stopifnot(is.array(x)||is.vector(x))
	if (is.vector(x)){
		modes <- c(length(x))
		num_modes <- 1L
		new("DelayedTensor", num_modes, modes, data = x)
	}
	else {
		modes <- dim(x)
		num_modes <- length(modes)
		dim1s <- which(modes==1)
		if (drop && (length(dim1s)>0)){
			modes <- modes[-dim1s]
			num_modes <- num_modes-length(dim1s)
			new("DelayedTensor",num_modes,modes,data=array(x,dim=modes))
		}
		else{
			new("DelayedTensor",num_modes,modes,data=x)
		}
	}
}

setGeneric(name="tperm",
def=function(tnsr,perm,...){standardGeneric("tperm")})

setMethod("tperm",signature="DelayedTensor",
definition=function(tnsr,...){
	as.delayed.tensor(aperm(tnsr@data,...))
})

setGeneric(name="vec",def=function(tnsr){standardGeneric("vec")})

setMethod("vec",signature="DelayedTensor",

definition=function(tnsr){
	.vec(tnsr@data)
})
.vec <- function(x){
	as.vector(x)
}

setGeneric(name="as.delayed.vector",def=function(tnsr){standardGeneric("as.delayed.vector")})

setMethod("as.delayed.vector",signature="DelayedTensor",

definition=function(tnsr){
	.as.delayed.vector(tnsr@data)
})
.as.delayed.vector <- function(x){
	as.vector(x)
}


# Ref
# https://github.com/cran/rTensor/blob/master/R/rTensor_Class.R