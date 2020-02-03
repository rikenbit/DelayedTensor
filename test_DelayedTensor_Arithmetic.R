library("rTensor")

load("faces_tnsr.RData")
source("DelayedTensor_Arithmetic.R")
source("DelayedTensor_Class.R")
source("DelayedTensor_Decomp.R")
source("DelayedTensor_Misc.R")

set.seed(1234)
tnsr <- rTensor::rand_tensor()
set.seed(1234)
dtnsr <- rand_tensor()

context("### 1. +x ###\n")
# expect_identical(
#     +tnsr@data,
#     (+dtnsr)@data)


context("### 2. -x ###\n")
# expect_identical(
#     -tnsr@data,
#     (-dtnsr)@data)


context("### 3. x + y ###\n")
expect_identical(
    (tnsr + 2)@data,
    (dtnsr + 2)@data)

expect_identical(
    (tnsr + tnsr)@data,
    (dtnsr + dtnsr)@data)


context("### 4. x - y ###\n")
expect_identical(
    (tnsr - 2)@data,
    (dtnsr - 2)@data)

expect_identical(
    (tnsr - tnsr)@data,
    (dtnsr - dtnsr)@data)


context("### 5. x * y ###\n")
expect_identical(
    (tnsr * 2)@data,
    (dtnsr * 2)@data)

expect_identical(
    (tnsr * tnsr)@data,
    (dtnsr * dtnsr)@data)


context("### 6. x / y ###\n")
expect_identical(
    (tnsr / 2)@data,
    (dtnsr / 2)@data)

expect_identical(
    (tnsr / tnsr)@data,
    (dtnsr / dtnsr)@data)


context("### 7. x^y ###\n")
expect_identical(
    (tnsr^2)@data,
    (dtnsr^2)@data)

expect_identical(
    (tnsr^tnsr)@data,
    (dtnsr^dtnsr)@data)


context("### 8. x %*% y ###\n")
# expect_identical(
#     tnsr[1,,]@data %*% t(tnsr[1,,]@data),
#     (dtnsr[1,,] %*% t(dtnsr[1,,]))@data)


context("### 9. outer / %o% ###\n")
# expect_identical(
#     tnsr[1,2,]@data %o% tnsr[1,2,]@data,
#     (dtnsr[1,2,] %o% dtnsr[1,2,])@data)

# expect_identical(
#     tnsr[1,,]@data %o% tnsr[1,,]@data,
#     (dtnsr[1,,] %o% dtnsr[1,,])@data)

# expect_identical(
#     rTensor::outer(tnsr[1,2,]@data, tnsr[1,2,]@data, "*"),
#     (outer(dtnsr[1,2,], dtnsr[1,2,], "*"))@data)

# expect_identical(
#     rTensor::outer(tnsr[1,,]@data, tnsr[1,,]@data, "+"),
#     (outer(dtnsr[1,,], dtnsr[1,,], "+"))@data)

# expect_identical(
#     rTensor::outer(tnsr[1,,]@data, tnsr[1,,]@data, "*"),
#     (outer(dtnsr[1,,], dtnsr[1,,], "*"))@data)

# expect_identical(
#     rTensor::outer(tnsr[1,,]@data, tnsr[1,,]@data, "+"),
#     (outer(dtnsr[1,,], dtnsr[1,,], "+"))@data)


context("### 10. x %/% y ###\n")
expect_identical(
    (tnsr %/% 2)@data,
    (dtnsr %/% 2)@data)

expect_identical(
    (tnsr %/% tnsr)@data,
    (dtnsr %/% dtnsr)@data)


context("### 11. x %% y ###\n")
expect_identical(
    (tnsr %% 2)@data,
    (dtnsr %% 2)@data)

expect_identical(
    (tnsr %% tnsr)@data,
    (dtnsr %% dtnsr)@data)


context("### 12. sqrt(x) ###\n")
# expect_identical(
#     sqrt(tnsr@data),
#     sqrt(dtnsr)@data)


context("### 13. abs(x) ###\n")
# expect_identical(
#     abs(tnsr@data),
#     abs(dtnsr)@data)


context("### 14. exp(x) ###\n")
# expect_identical(
#     exp(tnsr@data),
#     exp(dtnsr)@data)


context("### 15. log(x) ###\n")
# expect_identical(
#     log(tnsr@data),
#     log(dtnsr)@data)


context("### 16. log10(x) ###\n")
# expect_identical(
#     log10(tnsr@data),
#     log10(dtnsr)@data)


context("### 17. log2(x) ###\n")
# expect_identical(
#     log2(tnsr@data),
#     log2(dtnsr)@data)


context("### 18. ceiling(x) ###\n")
# expect_identical(
#     ceiling(tnsr@data),
#     ceiling(dtnsr)@data)


context("### 19. floor(x) ###\n")
# expect_identical(
#     floor(tnsr@data),
#     floor(dtnsr)@data)


context("### 20. round(x,digits=0) ###\n")
# expect_identical(
#     round(tnsr@data),
#     round(dtnsr)@data)
