# Setting
arr1 <- array(rbinom(2*3*4, 2, 0.05), dim=c(2,3,4))
arr2 <- array(rbinom(2*3*4, 2, 0.05), dim=c(2,3,4))
darr1 <- DelayedArray(arr1)
darr2 <- DelayedArray(arr2)

# unfold: vec, .reshapeIncNumbers1D
context("### unfold ###")
setSparse(FALSE)
res1 <- unfold(darr1, row_idx=1, col_idx=2:3)
setSparse(TRUE)
res2 <- unfold(darr1, row_idx=1, col_idx=2:3)
expect_equal(as.array(res1), as.array(res2))

# modeSum: read_block, .block_modesum
context("### modeSum ###")
setSparse(FALSE)
res1 <- modeSum(darr1, m=2)
setSparse(TRUE)
res2 <- modeSum(darr1, m=2)
expect_equal(as.array(res1), as.array(res2))

# modeMean: /
context("### modeMean ###")
setSparse(FALSE)
res1 <- modeMean(darr1, m=2)
setSparse(TRUE)
res2 <- modeMean(darr1, m=2)
expect_equal(as.array(res1), as.array(res2))

# innerProd: *
context("### innerProd ###")
setSparse(FALSE)
res1 <- innerProd(darr1, darr2)
setSparse(TRUE)
res2 <- innerProd(darr1, darr2)
expect_equal(res1, res2)

# vec: SparseArrayでかつOut-of-coreな書き方
context("### vec ###")
setSparse(FALSE)
res1 <- vec(darr1)
setSparse(TRUE)
res2 <- vec(darr1)
expect_equal(as.vector(res1), as.vector(res2))

# hadamard: .block_hadamard, read_block
context("### hadamard ###")
setSparse(FALSE)
res1 <- hadamard(darr1, darr2)
setSparse(TRUE)
res2 <- hadamard(darr1, darr2)
expect_equal(as.array(res1), as.array(res2))

# kronecker: .block_kronecker, read_block
context("### kronecker ###")
setSparse(FALSE)
res1 <- kronecker(darr1, darr2)
setSparse(TRUE)
res2 <- kronecker(darr1, darr2)
expect_equal(as.array(res1), as.array(res2))

# khatri_rao: .block_khatri_rao, read_block
context("### khatri_rao ###")
setSparse(FALSE)
res1 <- khatri_rao(darr1[,,1], darr2[,,1])
setSparse(TRUE)
res2 <- khatri_rao(darr1[,,1], darr2[,,1])
expect_equal(as.array(res1), as.array(res2))

# fold: vec, .reshapeIncNumbers1D
context("### fold ###")
setSparse(FALSE)
res1 <- fold(unfold(darr1, row_idx=1, col_idx=2:3),
    row_idx=1, col_idx=2:3, modes=dim(darr1))
setSparse(TRUE)
res2 <- fold(unfold(darr1, row_idx=1, col_idx=2:3),
    row_idx=1, col_idx=2:3, modes=dim(darr1))
expect_equal(as.array(res1), as.array(res2))

# modebind_list: .reshapeIncNumbers1D
context("### modebind_list ###")
setSparse(FALSE)
res1 <- modebind_list(list(darr1, darr2, darr1, darr2), m=2)
setSparse(TRUE)
res2 <- modebind_list(list(darr1, darr2, darr1, darr2), m=2)
expect_equal(as.array(res1), as.array(res2))
