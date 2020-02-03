library("rTensor")

load("faces_tnsr.RData")
source("DelayedTensor_Arithmetic.R")
source("DelayedTensor_Class.R")
source("DelayedTensor_Decomp.R")
source("DelayedTensor_Misc.R")

context("### 52. hadamard_list ###\n")
lizt <- list('mat1' = matrix(runif(40),ncol=4),
'mat2' = matrix(runif(40),ncol=4),
'mat3' = matrix(runif(40),ncol=4))

expect_identical(
    rTensor::hadamard_list(lizt),
    hadamard_list(lizt))


context("### 53. kronecker_list ###\n")
smalllizt <- list('mat1' = matrix(runif(12),ncol=4),
 'mat2' = matrix(runif(12),ncol=4),
 'mat3' = matrix(runif(12),ncol=4))

expect_identical(
    rTensor::kronecker_list(smalllizt),
    kronecker_list(smalllizt))


context("### 54. khatri_raoa ###\n")
A <- matrix(runif(12),ncol=4)
B <- matrix(runif(12),ncol=4)

expect_identical(
    rTensor::khatri_rao(A, B),
    khatri_rao(A, B))


context("### 55. khatori_rao_list ###\n")
expect_identical(
    rTensor::khatri_rao_list(smalllizt, reverse=FALSE),
    khatri_rao_list(smalllizt, reverse=FALSE))

expect_identical(
    rTensor::khatri_rao_list(smalllizt, reverse=TRUE),
    khatri_rao_list(smalllizt, reverse=TRUE))


context("### 56. ttm ###\n")
set.seed(1234)
tnsr <- new("Tensor", 3L, c(3L,4L,5L), data=runif(60))
set.seed(1234)
dtnsr <- new("DelayedTensor", 3L, c(3L,4L,5L), data=runif(60))
mat <- matrix(runif(50),ncol=5)

expect_identical(
    rTensor::ttm(tnsr, mat, m=3)@data,
    ttm(dtnsr, mat, m=3)@data)


context("### 57. ttl ###\n")
tnsr <- new("Tensor",3L,c(3L,4L,5L),data=runif(60))
lizt <- list('mat1' = matrix(runif(30),ncol=3),
    'mat2' = matrix(runif(40),ncol=4),
    'mat3' = matrix(runif(50),ncol=5))

expect_identical(
    rTensor::ttl(tnsr, lizt, ms=c(1,2,3))@data,
    ttl(tnsr, lizt, ms=c(1,2,3))@data)


context("### 58. t_mult ###\n")
set.seed(1234)
tnsr <- new("Tensor",3L,c(3L,4L,5L),data=runif(60))
set.seed(1234)
tnsr2 <- new("Tensor",3L,c(4L,3L,5L),data=runif(60))

set.seed(1234)
dtnsr <- new("DelayedTensor",3L,c(3L,4L,5L),data=runif(60))
set.seed(1234)
dtnsr2 <- new("DelayedTensor",3L,c(4L,3L,5L),data=runif(60))

expect_identical(
    t_mult(tnsr, tnsr2)@data,
    t_mult(dtnsr, dtnsr2)@data)


context("### 59. rand_tensor ###\n")
set.seed(1234)
A <- rTensor::rand_tensor(modes=c(3,4,5),drop=FALSE)
set.seed(1234)
B <- rand_tensor(modes=c(3,4,5),drop=FALSE)

expect_identical(
    A@data,
    B@data)

set.seed(1234)
C <- rTensor::rand_tensor(modes=c(3,4,1),drop=TRUE)
set.seed(1234)
D <- rand_tensor(modes=c(3,4,1),drop=TRUE)

expect_identical(
    C@data,
    D@data)


context("### 60. fold ###\n")
set.seed(1234)
matT3 <- rTensor::unfold(new("Tensor",3L,c(3L,4L,5L),data=runif(60)),
	row_idx=2, col_idx=c(3,1))
tnsr <- rTensor::fold(matT3@data, row_idx=2, col_idx=c(3,1), modes=c(3,4,5))

set.seed(1234)
dmatT3 <- unfold(new("Tensor",3L,c(3L,4L,5L),data=runif(60)),
	row_idx=2, col_idx=c(3,1))
dtnsr <- fold(matT3@data, row_idx=2, col_idx=c(3,1), modes=c(3,4,5))

expect_identical(
    tnsr@data,
    dtnsr@data)


context("### 61. k_fold ###\n")
set.seed(1234)
tnsr <- new("Tensor", 3L, c(3L,4L,5L), data=runif(60))
matT2 <- rTensor::k_unfold(tnsr, m=2)

expect_identical(
    rTensor::k_fold(matT2@data, m=2, modes=c(3,4,5)),
    tnsr)

set.seed(1234)
dtnsr <- new("DelayedTensor", 3L, c(3L,4L,5L), data=runif(60))
dmatT2 <- k_unfold(dtnsr, m=2)

expect_identical(
    k_fold(dmatT2@data, m=2, modes=c(3,4,5)),
    dtnsr)


context("### 62. unmatvec ###\n")
set.seed(1234)
tnsr <- new("Tensor", 3L, c(3L,4L,5L), data=runif(60))
matT1 <- rTensor::matvec(tnsr)

expect_identical(
    rTensor::unmatvec(matT1@data, modes=c(3,4,5)),
    tnsr)

set.seed(1234)
dtnsr <- new("DelayedTensor", 3L, c(3L,4L,5L), data=runif(60))
dmatT1 <- matvec(tnsr)

expect_identical(
    unmatvec(dmatT1@data, modes=c(3,4,5)),
    dtnsr)


context("### 63. rs_fold ###\n")
set.seed(1234)
tnsr <- new("Tensor", 3L, c(3L,4L,5L), data=runif(60))
matT2 <- rTensor::rs_unfold(tnsr, m=2)

expect_identical(
    rTensor::rs_fold(matT2@data, m=2, modes=c(3,4,5)),
    tnsr)

set.seed(1234)
dtnsr <- new("DelayedTensor", 3L, c(3L,4L,5L), data=runif(60))
dmatT2 <- rs_unfold(tnsr, m=2)

expect_identical(
    rs_fold(dmatT2@data, m=2, modes=c(3,4,5)),
    dtnsr)


context("### 64. cs_fold ###\n")
set.seed(1234)
tnsr <- new("Tensor", 3L, c(3L,4L,5L), data=runif(60))
matT1 <- rTensor::cs_unfold(tnsr, m=3)

expect_identical(
    rTensor::cs_fold(matT1@data, m=3, modes=c(3,4,5)),
    tnsr)

set.seed(1234)
dtnsr <- new("DelayedTensor", 3L, c(3L,4L,5L), data=runif(60))
dmatT1 <- cs_unfold(tnsr, m=3)

expect_identical(
    cs_fold(dmatT1@data, m=3, modes=c(3,4,5)),
    dtnsr)


context("### 65. .ifft ###\n")
set.seed(1234)
A <- rTensor:::.ifft(matrix(runif(3*5), nrow=3, ncol=5))
set.seed(1234)
B <- .ifft(matrix(runif(3*5), nrow=3, ncol=5))

expect_identical(
    A,
    B)


context("### 66. .superdiagonal_tensor ###\n")
expect_identical(
    rTensor:::.superdiagonal_tensor(3, 3)@data,
    .superdiagonal_tensor(3, 3)@data)
