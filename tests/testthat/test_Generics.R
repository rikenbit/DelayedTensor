# Setting
arr <- array(runif(2*3*4), dim=c(2,3,4))
tnsr <- as.tensor(arr)
darr <- DelayedArray(arr)
mat <- matrix(runif(10), nrow=5, ncol=2)
dmat <- DelayedArray(mat)

context("### unfold ###\n")
expect_equal(
    rTensor::unfold(tnsr, row_idx=1, col_idx=c(2, 3))@data,
    as.array(unfold(darr, row_idx=1, col_idx=c(2, 3))))

expect_equal(
    rTensor::unfold(tnsr, row_idx=1, col_idx=c(3, 2))@data,
    as.array(unfold(darr, row_idx=1, col_idx=c(3, 2))))

expect_equal(
    rTensor::unfold(tnsr, row_idx=2, col_idx=c(1, 3))@data,
    as.array(unfold(darr, row_idx=2, col_idx=c(1, 3))))

expect_equal(
    rTensor::unfold(tnsr, row_idx=2, col_idx=c(3, 1))@data,
    as.array(unfold(darr, row_idx=2, col_idx=c(3, 1))))

expect_equal(
    rTensor::unfold(tnsr, row_idx=3, col_idx=c(1, 2))@data,
    as.array(unfold(darr, row_idx=3, col_idx=c(1, 2))))

expect_equal(
    rTensor::unfold(tnsr, row_idx=3, col_idx=c(2, 1))@data,
    as.array(unfold(darr, row_idx=3, col_idx=c(2, 1))))

context("### k_unfold ###\n")
expect_equal(
    rTensor::k_unfold(tnsr, m=1)@data,
    as.array(k_unfold(darr, m=1)))

expect_equal(
    rTensor::k_unfold(tnsr, m=2)@data,
    as.array(k_unfold(darr, m=2)))

expect_equal(
    rTensor::k_unfold(tnsr, m=3)@data,
    as.array(k_unfold(darr, m=3)))

context("### matvec ###\n")
expect_equal(
    rTensor::matvec(tnsr)@data,
    as.array(matvec(darr)))

context("### rs_unfold ###\n")
expect_equal(
    rTensor::rs_unfold(tnsr, m=1)@data,
    as.array(rs_unfold(darr, m=1)))

expect_equal(
    rTensor::rs_unfold(tnsr, m=2)@data,
    as.array(rs_unfold(darr, m=2)))

expect_equal(
    rTensor::rs_unfold(tnsr, m=3)@data,
    as.array(rs_unfold(darr, m=3)))

context("### cs_unfold ###\n")
expect_equal(
    rTensor::cs_unfold(tnsr, m=1)@data,
    as.array(cs_unfold(darr, m=1)))

expect_equal(
    rTensor::cs_unfold(tnsr, m=2)@data,
    as.array(cs_unfold(darr, m=2)))

expect_equal(
    rTensor::cs_unfold(tnsr, m=3)@data,
    as.array(cs_unfold(darr, m=3)))

context("### modeSum ###\n")
expect_equal(
    rTensor::modeSum(tnsr, m=1, drop=FALSE)@data,
    as.array(modeSum(darr, m=1, drop=FALSE)))

expect_equal(
    rTensor::modeSum(tnsr, m=2, drop=FALSE)@data,
    as.array(modeSum(darr, m=2, drop=FALSE)))

expect_equal(
    rTensor::modeSum(tnsr, m=3, drop=FALSE)@data,
    as.array(modeSum(darr, m=3, drop=FALSE)))

expect_equal(
    rTensor::modeSum(tnsr, m=1, drop=TRUE)@data,
    as.array(modeSum(darr, m=1, drop=TRUE)))

expect_equal(
    rTensor::modeSum(tnsr, m=2, drop=TRUE)@data,
    as.array(modeSum(darr, m=2, drop=TRUE)))

expect_equal(
    rTensor::modeSum(tnsr, m=3, drop=TRUE)@data,
    as.array(modeSum(darr, m=3, drop=TRUE)))

context("### modeMean ###\n")
expect_equal(
    rTensor::modeMean(tnsr, m=1, drop=FALSE)@data,
    as.array(modeMean(darr, m=1, drop=FALSE)))

expect_equal(
    rTensor::modeMean(tnsr, m=2, drop=FALSE)@data,
    as.array(modeMean(darr, m=2, drop=FALSE)))

expect_equal(
    rTensor::modeMean(tnsr, m=3, drop=FALSE)@data,
    as.array(modeMean(darr, m=3, drop=FALSE)))

expect_equal(
    rTensor::modeMean(tnsr, m=1, drop=TRUE)@data,
    as.array(modeMean(darr, m=1, drop=TRUE)))

expect_equal(
    rTensor::modeMean(tnsr, m=2, drop=TRUE)@data,
    as.array(modeMean(darr, m=2, drop=TRUE)))

expect_equal(
    rTensor::modeMean(tnsr, m=3, drop=TRUE)@data,
    as.array(modeMean(darr, m=3, drop=TRUE)))

context("### fnorm ###\n")
expect_equal(
    rTensor::fnorm(tnsr),
    fnorm(darr))

context("### innerProd ###\n")
expect_equal(
    rTensor::innerProd(tnsr, tnsr),
    innerProd(darr, darr))

context("### outerProd ###\n")
expect_equal(
    dim(outerProd(darr, darr)),
    c(dim(darr), dim(darr)))

context("### vec ###\n")
expect_equal(
	as.vector(rTensor::vec(tnsr)),
	as.vector(vec(darr)))

context("### hadamard ###\n")
expect_equal(
    tnsr@data * tnsr@data,
    as.array(hadamard(darr, darr)))

context("### kronecker ###\n")
expect_equal(
    base::kronecker(tnsr@data, tnsr@data),
    as.array(kronecker(darr, darr)))

context("### khatri_rao ###\n")
expect_equal(
    rTensor::khatri_rao(tnsr@data[,,1], tnsr@data[,,1]),
    as.array(khatri_rao(darr[,,1], darr[,,1])))

context("### fold ###\n")
tnsr2 <- rTensor::fold(
    rTensor::unfold(tnsr, row_idx=2, col_idx=c(3,1))@data,
    row_idx=2, col_idx=c(3,1), modes=c(2,3,4))
darr2 <- fold(unfold(darr, row_idx=2, col_idx=c(3,1)),
    row_idx=2, col_idx=c(3,1), modes=c(2,3,4))
expect_equal(
    tnsr@data,
    tnsr2@data,
    as.array(darr2))

context("### k_fold ###\n")
expect_equal(
    rTensor::k_fold(rTensor::k_unfold(tnsr, m=2)@data, m=2, modes=c(2,3,4))@data,
    tnsr@data,
    as.array(k_fold(k_unfold(darr, m=2), m=2, modes=c(2,3,4))))

context("### unmatvec ###\n")
expect_equal(
    rTensor::unmatvec(rTensor::matvec(tnsr)@data, modes=c(2,3,4))@data,
    tnsr@data,
    as.array(unmatvec(matvec(darr), modes=c(2,3,4))))

context("### rs_fold ###\n")
expect_equal(
    rTensor::rs_fold(rTensor::rs_unfold(tnsr, m=2)@data, m=2, modes=c(2,3,4))@data,
    tnsr@data,
    as.array(rs_fold(rs_unfold(darr, m=2), m=2, modes=c(2,3,4))))

context("### cs_fold ###\n")
expect_equal(
    rTensor::cs_fold(rTensor::cs_unfold(tnsr, m=3)@data, m=3, modes=c(2,3,4))@data,
    tnsr@data,
    as.array(cs_fold(cs_unfold(darr, m=3), m=3, modes=c(2,3,4))))

context("### ttm ###\n")
expect_equal(
    rTensor::ttm(tnsr, mat, m=1)@data,
    as.array(ttm(darr, dmat, m=1)))

context("### diag (getter) ###\n")
expect_equal(
    c(arr[1,1,1], arr[2,2,2]),
    as.vector(diag(darr)))

context("### diag<- (setter) ###\n")
arr[1,1,1] <- 111
arr[2,2,2] <- 222
diag(darr) <- c(111 ,222)
expect_equal(
    c(arr[1,1,1], arr[2,2,2]),
    as.vector(diag(darr)))
