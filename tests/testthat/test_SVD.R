# Setting
mat <- matrix(runif(5*6), nrow=5, ncol=6)
dmat <- DelayedArray(mat)

context("### svd ###\n")
for(k in seq(min(dim(dmat)))){
	out_m <- svd(mat, nu=k, nv=k)
	out_d <- .svd(dmat, k=k)
	expect_equal(
		dim(out_m$u),
		dim(out_d$u))
	expect_equal(
		dim(out_m$v),
		dim(out_d$v))
}
