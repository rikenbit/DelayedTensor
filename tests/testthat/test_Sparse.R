# # Setting
# arr <- array(runif(2*3*4), dim=c(2,3,4))
# ddarr <- DelayedArray(arr)
# sdarr <- dense2sparse(arr)

# # AllGenerics.R
# context("### unfold ###")
# print("unfold")
# expect_identical(
#     is_sparse(unfold(ddarr, row_idx=1, col_idx=c(2, 3, 4))),
#     is_sparse(unfold(sdarr, row_idx=1, col_idx=c(2, 3, 4))))

# context("### modeSum ###")
# print("modeSum")
# expect_identical(
#     rTensor::modeSum(tnsr, m=1, drop=FALSE)@data,
#     as.array(modeSum(darr, m=1, drop=FALSE)))

# expect_identical(
#     rTensor::modeSum(tnsr, m=2, drop=FALSE)@data,
#     as.array(modeSum(darr, m=2, drop=FALSE)))

# expect_identical(
#     rTensor::modeSum(tnsr, m=3, drop=FALSE)@data,
#     as.array(modeSum(darr, m=3, drop=FALSE)))

# expect_identical(
#     rTensor::modeSum(tnsr, m=1, drop=TRUE)@data,
#     as.array(modeSum(darr, m=1, drop=TRUE)))

# expect_identical(
#     rTensor::modeSum(tnsr, m=2, drop=TRUE)@data,
#     as.array(modeSum(darr, m=2, drop=TRUE)))

# expect_identical(
#     rTensor::modeSum(tnsr, m=3, drop=TRUE)@data,
#     as.array(modeSum(darr, m=3, drop=TRUE)))

# context("### innerProd ###")
# print("innerProd")
# expect_equal(
#     rTensor::innerProd(tnsr, tnsr),
#     innerProd(darr, darr))

# context("### vec ###")
# print("vec")
# expect_equal(
#     rTensor::vec(tnsr),
#     as.vector(vec(darr)))

# context("### kronecker ###")
# expect_equal(
#     base::kronecker(arr, arr),
#     as.array(kronecker(darr, darr)))

# context("### fold ###")
# print("fold")
# matT4 <- rTensor::unfold(tnsr, row_idx=1, col_idx=2:4)
# dmatT4 <- unfold(darr, row_idx=1, col_idx=2:4)
# tnsr_2 <- rTensor::fold(matT4,
#     row_idx=1, col_idx=2:4, modes=c(2,3,4,5))
# darr_2 <- fold(dmatT4,
#     row_idx=1, col_idx=2:4, modes=c(2,3,4,5))
# expect_identical(
#     arr,
#     tnsr_2@data,
#     as.array(darr_2))

# matT4 <- rTensor::unfold(tnsr, row_idx=2, col_idx=c(3,1,4))
# dmatT4 <- unfold(darr, row_idx=2, col_idx=c(3,1,4))
# tnsr_3 <- rTensor::fold(matT4,
#     row_idx=2, col_idx=c(3,1,4), modes=c(2,3,4,5))
# darr_3 <- fold(dmatT4,
#     row_idx=2, col_idx=c(3,1,4), modes=c(2,3,4,5))
# expect_identical(
#     arr,
#     tnsr_3@data,
#     as.array(darr_3))

# matT4 <- rTensor::unfold(tnsr, row_idx=3, col_idx=c(2,1,4))
# dmatT4 <- unfold(darr, row_idx=3, col_idx=c(2,1,4))
# tnsr_4 <- rTensor::fold(matT4,
#     row_idx=3, col_idx=c(2,1,4), modes=c(2,3,4,5))
# darr_4 <- fold(dmatT4,
#     row_idx=3, col_idx=c(2,1,4), modes=c(2,3,4,5))
# expect_identical(
#     arr,
#     tnsr_4@data,
#     as.array(darr_4))

# matT4 <- rTensor::unfold(tnsr, row_idx=4, col_idx=c(3,2,1))
# dmatT4 <- unfold(darr, row_idx=4, col_idx=c(3,2,1))
# tnsr_5 <- rTensor::fold(matT4@data,
#     row_idx=4, col_idx=c(3,2,1), modes=c(2,3,4,5))
# darr_5 <- fold(dmatT4,
#     row_idx=4, col_idx=c(3,2,1), modes=c(2,3,4,5))
# expect_identical(
#     arr,
#     tnsr_5@data,
#     as.array(darr_5))

# context("### diag ###")
# print("diag")
# expect_identical(
#     c(arr[1,1,1,1], arr[2,2,2,2]),
#     as.vector(diag(darr)))

# context("### diag<- ###")
# print("diag")
# arr_2 <- arr
# darr_2 <- darr
# arr_2[1,1,1,1] <- 111
# arr_2[2,2,2,2] <- 222
# diag(darr_2) <- c(111, 222)
# expect_identical(
#     c(arr_2[1,1,1,1], arr_2[2,2,2,2]),
#     as.vector(diag(darr_2)))

# # Misc.R
# context("### DelayedDiagonalArray ###")
# print("DelayedDiagonalArray")
# A <- DelayedDiagonalArray(c(3,4,5,6))
# B <- DelayedDiagonalArray(c(3,4,5,6), 1:3)

# expect_equal(A[1,1,1,1], 1)
# expect_equal(A[2,2,2,2], 1)
# expect_equal(A[3,3,3,3], 1)
# expect_equal(A[3,4,5,6], 0)

# expect_equal(B[1,1,1,1], 1)
# expect_equal(B[2,2,2,2], 2)
# expect_equal(B[3,3,3,3], 3)
# expect_equal(B[3,4,5,6], 0)
