context("### 52. hadamard_list ###\n")
# tensorrには実装がない
lizt <- list(
    'mat1' = matrix(runif(40), ncol=4),
    'mat2' = matrix(runif(40), ncol=4),
    'mat3' = matrix(runif(40), ncol=4))

expect_identical(
    rTensor::hadamard_list(lizt),
    hadamard_list(lizt))


context("### 53. kronecker_list ###\n")
# tensorrには実装がない
expect_identical(
    rTensor::kronecker_list(lizt),
    kronecker_list(lizt))


context("### 54. khatri_raoa ###\n")
# tensorrには実装がない
A <- matrix(runif(12), ncol=4)
B <- matrix(runif(12), ncol=4)

expect_identical(
    rTensor::khatri_rao(A, B),
    khatri_rao(A, B))


context("### 55. khatori_rao_list ###\n")
# tensorrには実装がない
expect_identical(
    rTensor::khatri_rao_list(lizt, reverse=FALSE),
    khatri_rao_list(lizt, reverse=FALSE))

expect_identical(
    rTensor::khatri_rao_list(lizt, reverse=TRUE),
    khatri_rao_list(lizt, reverse=TRUE))


context("### 56. ttm ###\n")
mat <- matrix(runif(50),ncol=5)

expect_identical(
    rTensor::ttm(tnsr, mat, m=3)@data,
    .sp_to_array(tensorr::ttm(stnsr, mat, 3)),
    ttm(dtnsr, mat, m=3)@data)


context("### 57. ttl ###\n")
# tensorrには実装がない
lizt2 <- list(
    'mat1' = matrix(runif(30), ncol=3),
    'mat2' = matrix(runif(40), ncol=4),
    'mat3' = matrix(runif(50), ncol=5))

expect_identical(
    rTensor::ttl(tnsr, lizt2, ms=c(1,2,3))@data,
    ttl(tnsr, lizt2, ms=c(1,2,3))@data)


context("### 58. t_mult ###\n")
# tensorrには実装がない
set.seed(1234)
tnsr2 <- new("Tensor", 3L, c(4L,3L,5L), data=runif(60))
set.seed(1234)
dtnsr2 <- new("DelayedTensor", 3L, c(4L,3L,5L), data=runif(60))

expect_identical(
    t_mult(tnsr, tnsr2)@data,
    t_mult(dtnsr, dtnsr2)@data)


context("### 59. rand_tensor ###\n")
# tensorrはrTensorを頼るしかない
set.seed(1234)
A <- rTensor::rand_tensor(modes=c(3,4,5), drop=FALSE)
B <- tensorr::as_sptensor(as_dtensor(A@data))
set.seed(1234)
C <- rand_tensor(modes=c(3,4,5), drop=FALSE)

expect_identical(
    A@data,
    .sp_to_array(B),
    C@data)

set.seed(1234)
D <- rTensor::rand_tensor(modes=c(3,4,1), drop=TRUE)
E <- tensorr::as_sptensor(as_dtensor(D@data))
set.seed(1234)
F <- rand_tensor(modes=c(3,4,1), drop=TRUE)

expect_identical(
    D@data,
    matrix(as.matrix(E), nrow=3),
    F@data)


context("### 60. fold ###\n")
# tensorrにはcol_idxオプションがない
matT3 <- rTensor::unfold(tnsr, row_idx=2, col_idx=c(3,1))
tnsr <- rTensor::fold(matT3@data, row_idx=2, col_idx=c(3,1), modes=c(3,4,5))

smatT3 <- tensorr::unfold(stnsr, 2)
stnsr <- tensorr::refold(smatT3)

dmatT3 <- unfold(dtnsr, row_idx=2, col_idx=c(3,1))
dtnsr <- fold(matT3@data, row_idx=2, col_idx=c(3,1), modes=c(3,4,5))

expect_identical(
    tnsr@data,
    .sp_to_array(stnsr),
    dtnsr@data)


context("### 61. k_fold ###\n")
matT2 <- rTensor::k_unfold(tnsr, m=2)

expect_identical(
    rTensor::k_fold(matT2@data, m=2, modes=c(3,4,5)),
    tnsr)

smatT2 <- tensorr::unfold(stnsr, m=2)

expect_identical(
    tensorr::refold(smatT2),
    stnsr)

dmatT2 <- k_unfold(dtnsr, m=2)

expect_identical(
    k_fold(dmatT2@data, m=2, modes=c(3,4,5)),
    dtnsr)


context("### 62. unmatvec ###\n")
# tensorrには実装がない
matT1 <- rTensor::matvec(tnsr)

expect_identical(
    rTensor::unmatvec(matT1@data, modes=c(3,4,5)),
    tnsr)

dmatT1 <- matvec(tnsr)

expect_identical(
    unmatvec(dmatT1@data, modes=c(3,4,5)),
    dtnsr)


context("### 63. rs_fold ###\n")
matT2 <- rTensor::rs_unfold(tnsr, m=2)

expect_identical(
    rTensor::rs_fold(matT2@data, m=2, modes=c(3,4,5)),
    tnsr)

smatT2 <- tensorr::unfold(stnsr, 2)

expect_identical(
    tensorr::refold(smatT2),
    stnsr)

dmatT2 <- rs_unfold(tnsr, m=2)

expect_identical(
    rs_fold(dmatT2@data, m=2, modes=c(3,4,5)),
    dtnsr)


context("### 64. cs_fold ###\n")
# tensorrには実装がない
matT1 <- rTensor::cs_unfold(tnsr, m=3)

expect_identical(
    rTensor::cs_fold(matT1@data, m=3, modes=c(3,4,5)),
    tnsr)

dmatT1 <- cs_unfold(tnsr, m=3)

expect_identical(
    cs_fold(dmatT1@data, m=3, modes=c(3,4,5)),
    dtnsr)


context("### 65. .ifft ###\n")
# tensorrには実装がない
set.seed(1234)
A <- rTensor:::.ifft(matrix(runif(3*5), nrow=3, ncol=5))
set.seed(1234)
B <- .ifft(matrix(runif(3*5), nrow=3, ncol=5))

expect_identical(
    A,
    B)


context("### 66. .superdiagonal_tensor ###\n")
# tensorrには実装がない
expect_identical(
    rTensor:::.superdiagonal_tensor(3, 3)@data,
    .sp_to_array(
        sptensor(
            list(c(1,1,1), c(2,2,2), c(3,3,3)),
            rep(1, 3),
            c(3, 3, 3))),
    .superdiagonal_tensor(3, 3)@data)
