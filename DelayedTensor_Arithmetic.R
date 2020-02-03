setClass("DelayedTensor",
representation(num_modes = "integer", modes = "integer", data="array"),
validity = function(object){
	num_modes <- object@num_modes
	modes <- object@modes
	errors <- character()
	if (any(modes <= 0)){
		msg <- "'modes' must contain strictly positive values; if any mode is 1, consider a smaller num_modes"
		errors <- c(errors, msg)
	}
	if(length(errors)==0) TRUE else errors
})


setMethod(f="initialize",
signature="DelayedTensor",
definition = function(.Object, num_modes=NULL, modes=NULL, data=NULL){
	if(is.null(num_modes)){
		if (is.vector(data)) num_modes <- 1L
		else{num_modes <- length(dim(data))}
	}
	if(is.null(modes)){
		if (is.vector(data)) modes <- length(data)
		else{modes <- dim(data)}
	}
	.Object@num_modes <- num_modes
	.Object@modes <- modes
	if (is.vector(data)){
		.Object@data <- array(data,dim=modes)
	}else{
		.Object@data <- data
	}
	validObject(.Object)
	.Object
})

setMethod(f="initialize",
signature="DelayedTensor",
definition = function(.Object, num_modes=NULL, modes=NULL, data=NULL){
	if(is.null(num_modes)){
		if (is.vector(data)) num_modes <- 1L
		else{num_modes <- length(dim(data))}
	}
	if(is.null(modes)){
		if (is.vector(data)) modes <- length(data)
		else{modes <- dim(data)}
	}
	.Object@num_modes <- num_modes
	.Object@modes <- modes
	if (is.vector(data)){
		.Object@data <- array(data,dim=modes)
	}else{
		.Object@data <- data
	}
	validObject(.Object)
	.Object
})

setMethod("Ops", signature(e1="DelayedTensor", e2="DelayedTensor"),
definition=function(e1,e2){
	e1@data<-callGeneric(e1@data, e2@data)
	validObject(e1)
	e1
})
setMethod("Ops", signature(e1="DelayedTensor", e2="array"),
definition=function(e1,e2){
	e1@data<-callGeneric(e1@data,e2)
	validObject(e1)
	e1
})
setMethod("Ops", signature(e1="array", e2="DelayedTensor"),
definition=function(e1,e2){
	e2@data<-callGeneric(e1,e2@data)
	validObject(e2)
	e2
})
setMethod("Ops", signature(e1="DelayedTensor", e2="numeric"),
definition=function(e1,e2){
	e1@data<-callGeneric(e1@data,e2)
	validObject(e1)
	e1
})
setMethod("Ops", signature(e1="numeric", e2="DelayedTensor"),
definition=function(e1,e2){
	e2@data<-callGeneric(e1,e2@data)
	validObject(e2)
	e2
})
