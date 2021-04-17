#
# Generic/Method Definitions
#

# unfold
setGeneric("unfold",
	function(darr, row_idx, col_idx){standardGeneric("unfold")})
setMethod("unfold", signature(darr="DelayedArray"),
	function(darr, row_idx, col_idx){
		.unfold(darr, row_idx, col_idx)
	})
.unfold <- function(darr, row_idx, col_idx){
	#
	# ここに具体的な実装を書く
	#
}

# k_unfold
setGeneric("k_unfold",
	function(darr, m){standardGeneric("k_unfold")})
setMethod("k_unfold", signature(darr="DelayedArray"),
	function(darr, m){
		.k_unfold(darr, m)
	})
.k_unfold <- function(darr, m){
	#
	# ここに具体的な実装を書く
	#
}

# matvec
setGeneric("matvec",
	function(darr){standardGeneric("matvec")})
setMethod("matvec", signature(darr="DelayedArray"),
	function(darr){
		.matvec(darr)
	})
.matvec <- function(darr){
	#
	# ここに具体的な実装を書く
	#
}

# rs_unfold
setGeneric("rs_unfold",
	function(darr, m){standardGeneric("rs_unfold")})
setMethod("rs_unfold", signature(darr="DelayedArray"),
	function(darr, m){
		.rs_unfold(darr, m)
	})
.rs_unfold <- function(darr, m){
	#
	# ここに具体的な実装を書く
	#
}

# cs_unfold
setGeneric("cs_unfold",
	function(darr, m){standardGeneric("cs_unfold")})
setMethod("cs_unfold", signature(darr="DelayedArray"),
	function(darr, m){
		.cs_unfold(darr, m)
	})
.cs_unfold <- function(darr, m){
	#
	# ここに具体的な実装を書く
	#
}

# modeSum
setGeneric("modeSum",
	function(darr, m, drop){standardGeneric("modeSum")})
setMethod("modeSum", signature(darr="DelayedArray"),
	function(darr, m, drop){
		.modeSum(darr, m, drop)
	})
.modeSum <- function(darr, m, drop){
	#
	# ここに具体的な実装を書く
	#
}

# modeMean
setGeneric("modeMean",
	function(darr, m, drop){standardGeneric("modeMean")})
setMethod("modeMean", signature(darr="DelayedArray"),
	function(darr, m, drop){
		.modeMean(darr, m, drop)
	})
.modeMean <- function(darr, m, drop){
	#
	# ここに具体的な実装を書く
	#
}

# fnorm
setGeneric("fnorm",
	function(darr){standardGeneric("fnorm")})
setMethod("fnorm", signature(darr="DelayedArray"),
	function(darr){
		.fnorm(darr)
	})
.fnorm <- function(darr){
	#
	# ここに具体的な実装を書く
	#
}

# innerProd
setGeneric("innerProd",
	function(darr1, darr2){standardGeneric("innerProd")})
setMethod("innerProd", signature(darr1="DelayedArray", darr2="DelayedArray"),
	function(darr1, darr2){
		.innerProd(darr1, darr2)
	})
.innerProd <- function(darr1, darr2){
	#
	# ここに具体的な実装を書く
	#
}

# tperm
setGeneric("tperm",
	function(darr, perm, ...){standardGeneric("tperm")})
setMethod("tperm", signature(darr="DelayedArray"),
	function(darr, perm, ...){
		.tperm(darr, perm, ...)
	})
.tperm <- function(darr, perm, ...){
	#
	# ここに具体的な実装を書く
	#
}


# vec
setGeneric("vec",
	function(darr){standardGeneric("vec")})
setMethod("vec", signature(darr="DelayedArray"),
	function(darr){
		.vec(darr)
	})
.vec <- function(darr){
	#
	# ここに具体的な実装を書く
	#
}

# # Ref
# # https://github.com/cran/rTensor/blob/master/R/rTensor_Class.R