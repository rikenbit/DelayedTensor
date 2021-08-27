# Setting
arr4 <- array(runif(2*3*4*5), dim=c(2,3,4,5))
tnsr4 <- as.tensor(arr4)
darr4 <- DelayedArray(arr4)

# AllGenerics.R
context("### unfold ###")
print("unfold")
expect_equal(
    rTensor::unfold(tnsr4, row_idx=1, col_idx=c(2, 3, 4))@data,
    as.array(unfold(darr4, row_idx=1, col_idx=c(2, 3, 4))))
expect_equal(
    rTensor::unfold(tnsr4, row_idx=1, col_idx=c(2, 4, 3))@data,
    as.array(unfold(darr4, row_idx=1, col_idx=c(2, 4, 3))))
expect_equal(
    rTensor::unfold(tnsr4, row_idx=1, col_idx=c(3, 2, 4))@data,
    as.array(unfold(darr4, row_idx=1, col_idx=c(3, 2, 4))))
expect_equal(
    rTensor::unfold(tnsr4, row_idx=1, col_idx=c(3, 4, 2))@data,
    as.array(unfold(darr4, row_idx=1, col_idx=c(3, 4, 2))))
expect_equal(
    rTensor::unfold(tnsr4, row_idx=1, col_idx=c(4, 2, 3))@data,
    as.array(unfold(darr4, row_idx=1, col_idx=c(4, 2, 3))))
expect_equal(
    rTensor::unfold(tnsr4, row_idx=1, col_idx=c(4, 3, 2))@data,
    as.array(unfold(darr4, row_idx=1, col_idx=c(4, 3, 2))))

expect_equal(
    rTensor::unfold(tnsr4, row_idx=c(2, 4), col_idx=c(3, 1))@data,
    as.array(unfold(darr4, row_idx=c(2, 4), col_idx=c(3, 1))))

context("### modeSum ###")
print("modeSum")
expect_equal(
    rTensor::modeSum(tnsr4, m=1, drop=FALSE)@data,
    as.array(modeSum(darr4, m=1, drop=FALSE)))

expect_equal(
    rTensor::modeSum(tnsr4, m=2, drop=FALSE)@data,
    as.array(modeSum(darr4, m=2, drop=FALSE)))

expect_equal(
    rTensor::modeSum(tnsr4, m=3, drop=FALSE)@data,
    as.array(modeSum(darr4, m=3, drop=FALSE)))

expect_equal(
    rTensor::modeSum(tnsr4, m=4, drop=FALSE)@data,
    as.array(modeSum(darr4, m=4, drop=FALSE)))

expect_equal(
    rTensor::modeSum(tnsr4, m=1, drop=TRUE)@data,
    as.array(modeSum(darr4, m=1, drop=TRUE)))

expect_equal(
    rTensor::modeSum(tnsr4, m=2, drop=TRUE)@data,
    as.array(modeSum(darr4, m=2, drop=TRUE)))

expect_equal(
    rTensor::modeSum(tnsr4, m=3, drop=TRUE)@data,
    as.array(modeSum(darr4, m=3, drop=TRUE)))

expect_equal(
    rTensor::modeSum(tnsr4, m=4, drop=TRUE)@data,
    as.array(modeSum(darr4, m=4, drop=TRUE)))

context("### innerProd ###")
print("innerProd")
expect_equal(
    rTensor::innerProd(tnsr4, tnsr4),
    innerProd(darr4, darr4))

context("### vec ###")
print("vec")
expect_equal(
    rTensor::vec(tnsr4),
    as.vector(vec(darr4)))

context("### kronecker ###")
expect_equal(
    base::kronecker(arr4, arr4),
    as.array(kronecker(darr4, darr4)))

context("### fold ###")
print("fold")
matT4 <- rTensor::unfold(tnsr4, row_idx=1, col_idx=2:4)
dmatT4 <- unfold(darr4, row_idx=1, col_idx=2:4)
tnsr4_2 <- rTensor::fold(matT4,
    row_idx=1, col_idx=2:4, modes=c(2,3,4,5))
darr4_2 <- fold(dmatT4,
    row_idx=1, col_idx=2:4, modes=c(2,3,4,5))
expect_equal(
    arr4,
    tnsr4_2@data,
    as.array(darr4_2))

matT4 <- rTensor::unfold(tnsr4, row_idx=2, col_idx=c(3,1,4))
dmatT4 <- unfold(darr4, row_idx=2, col_idx=c(3,1,4))
tnsr4_3 <- rTensor::fold(matT4,
    row_idx=2, col_idx=c(3,1,4), modes=c(2,3,4,5))
darr4_3 <- fold(dmatT4,
    row_idx=2, col_idx=c(3,1,4), modes=c(2,3,4,5))
expect_equal(
    arr4,
    tnsr4_3@data,
    as.array(darr4_3))

matT4 <- rTensor::unfold(tnsr4, row_idx=3, col_idx=c(2,1,4))
dmatT4 <- unfold(darr4, row_idx=3, col_idx=c(2,1,4))
tnsr4_4 <- rTensor::fold(matT4,
    row_idx=3, col_idx=c(2,1,4), modes=c(2,3,4,5))
darr4_4 <- fold(dmatT4,
    row_idx=3, col_idx=c(2,1,4), modes=c(2,3,4,5))
expect_equal(
    arr4,
    tnsr4_4@data,
    as.array(darr4_4))

matT4 <- rTensor::unfold(tnsr4, row_idx=4, col_idx=c(3,2,1))
dmatT4 <- unfold(darr4, row_idx=4, col_idx=c(3,2,1))
tnsr4_5 <- rTensor::fold(matT4@data,
    row_idx=4, col_idx=c(3,2,1), modes=c(2,3,4,5))
darr4_5 <- fold(dmatT4,
    row_idx=4, col_idx=c(3,2,1), modes=c(2,3,4,5))
expect_equal(
    arr4,
    tnsr4_5@data,
    as.array(darr4_5))

context("### diag ###")
print("diag")
expect_equal(
    c(arr4[1,1,1,1], arr4[2,2,2,2]),
    as.vector(diag(darr4)))

context("### diag<- ###")
print("diag")
arr4_2 <- arr4
darr4_2 <- darr4
arr4_2[1,1,1,1] <- 111
arr4_2[2,2,2,2] <- 222
diag(darr4_2) <- c(111, 222)
expect_equal(
    c(arr4_2[1,1,1,1], arr4_2[2,2,2,2]),
    as.vector(diag(darr4_2)))

# Misc.R
context("### DelayedDiagonalArray ###")
print("DelayedDiagonalArray")
A <- DelayedDiagonalArray(c(3,4,5,6))
B <- DelayedDiagonalArray(c(3,4,5,6), 1:3)

expect_equal(A[1,1,1,1], 1)
expect_equal(A[2,2,2,2], 1)
expect_equal(A[3,3,3,3], 1)
expect_equal(A[3,4,5,6], 0)

expect_equal(B[1,1,1,1], 1)
expect_equal(B[2,2,2,2], 2)
expect_equal(B[3,3,3,3], 3)
expect_equal(B[3,4,5,6], 0)
