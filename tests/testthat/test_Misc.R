# Setting
lizt <- list(
    'mat1' = matrix(1:6, ncol=2),
    'mat2' = matrix(7:12, ncol=2),
    'mat3' = matrix(13:18, ncol=2))
lizt2 <- list(
    'mat1' = matrix(runif(30), ncol=3),
    'mat2' = matrix(runif(40), ncol=4),
    'mat3' = matrix(runif(50), ncol=5))
dlizt <- list(
    'mat1' = DelayedArray(lizt$mat1),
    'mat2' = DelayedArray(lizt$mat2),
    'mat3' = DelayedArray(lizt$mat3))
dlizt2 <- list(
    'mat1' = DelayedArray(lizt2$mat1),
    'mat2' = DelayedArray(lizt2$mat2),
    'mat3' = DelayedArray(lizt2$mat3))
set.seed(1234)
tnsr <- new("Tensor", 3L, c(3L, 4L, 5L), data=runif(60))
darr <- DelayedArray(tnsr@data)
mat <- matrix(runif(50), nrow=10, ncol=5)

context("### list_rep ###\n")
expect_equal(
    list_rep(tnsr, 3),
    list(tnsr, tnsr, tnsr))

context("### modebind_list/rbind_list ###\n")
expect_equal(
    as.array(rbind(dlizt[[1]], dlizt[[2]], dlizt[[3]])),
    as.array(modebind_list(dlizt, m=1)),
    as.array(rbind_list(dlizt)))

context("### modebind_list/cbind_list ###\n")
expect_equal(
    as.array(cbind(dlizt[[1]], dlizt[[2]], dlizt[[3]])),
    as.array(modebind_list(dlizt, m=2)),
    as.array(cbind_list(dlizt)))

context("### hadamard_list ###\n")
expect_equal(
    rTensor::hadamard_list(lizt),
    as.array(hadamard_list(dlizt)))

context("### kronecker_list ###\n")
expect_equal(
    rTensor::kronecker_list(lizt),
    as.array(kronecker_list(dlizt)))

context("### khatori_rao_list ###\n")
expect_equal(
    rTensor::khatri_rao_list(lizt, reverse=FALSE),
    as.array(khatri_rao_list(dlizt, reverse=FALSE)))
expect_equal(
    rTensor::khatri_rao_list(lizt, reverse=TRUE),
    as.array(khatri_rao_list(dlizt, reverse=TRUE)))

context("### ttl ###\n")
expect_equal(
    rTensor::ttl(tnsr, lizt2, ms=c(1,2,3))@data,
    as.array(ttl(darr, dlizt2, ms=c(1,2,3))))

context("### DelayedDiagonalArray ###\n")
E <- DelayedDiagonalArray(c(3,4,5))
expect_equal(E[1,1,1], 1)
expect_equal(E[2,2,2], 1)
expect_equal(E[3,3,3], 1)
expect_equal(E[3,4,5], 0)

F <- DelayedDiagonalArray(c(3,4,5), 1:3)
expect_equal(F[1,1,1], 1)
expect_equal(F[2,2,2], 2)
expect_equal(F[3,3,3], 3)
expect_equal(F[3,4,5], 0)
