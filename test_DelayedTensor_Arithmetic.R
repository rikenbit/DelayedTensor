context("### 1. +x ###\n")
# expect_identical(
#     +tnsr@data,
#     .sp_to_array(+stnsr),
#     (+dtnsr)@data)


context("### 2. -x ###\n")
# expect_identical(
#     -tnsr@data,
#     .sp_to_array(-stnsr),
#     (-dtnsr)@data)


context("### 3. x + y ###\n")
options(warn=-1)

expect_identical(
    (tnsr + 2)@data,
    .sp_to_array(stnsr + 2),
    (dtnsr + 2)@data)

options(warn=1)

expect_identical(
    (tnsr + tnsr)@data,
    .sp_to_array(stnsr + stnsr),
    (dtnsr + dtnsr)@data)


context("### 4. x - y ###\n")
options(warn=-1)

expect_identical(
    (tnsr - 2)@data,
    .sp_to_array(stnsr - 2),
    (dtnsr - 2)@data)

options(warn=1)

expect_identical(
    (tnsr - tnsr)@data,
    .sp_to_array(stnsr - stnsr),
    (dtnsr - dtnsr)@data)



context("### 5. x * y ###\n")
expect_identical(
    (tnsr * 2)@data,
    .sp_to_array(stnsr * 2),
    (dtnsr * 2)@data)

expect_identical(
    (tnsr * tnsr)@data,
    .sp_to_array(stnsr * stnsr),
    (dtnsr * dtnsr)@data)


context("### 6. x / y ###\n")
expect_identical(
    (tnsr / 2)@data,
    .sp_to_array(stnsr / 2),
    (dtnsr / 2)@data)

# tensorrではテンソル / テンソルはできない
expect_identical(
    (tnsr / tnsr)@data,
    (dtnsr / dtnsr)@data)


context("### 7. x^y ###\n")
expect_identical(
    (tnsr^2)@data,
    .sp_to_array(stnsr^2),
    (dtnsr^2)@data)

options(warn=-1)

expect_identical(
    (tnsr^tnsr)@data,
    .sp_to_array(stnsr^stnsr),
    (dtnsr^dtnsr)@data)

options(warn=1)


context("### 8. x %*% y ###\n")
# tensorrのサブテンソルでは行列積ができない
# なぜかunfoldの時だけ、sparseMatrix扱いになるが、
# 切り出したサブテンソルも2階だった場合は、sparseMatrixになってほしい...
# expect_identical(
#     tnsr[1,,]@data %*% t(tnsr[1,,]@data),
#     (dtnsr[1,,] %*% t(dtnsr[1,,]))@data)


context("### 9. outer / %o% ###\n")
expect_identical(
    tnsr[1,2,]@data %o% tnsr[1,2,]@data,
    stnsr[1,2,] %o% stnsr[1,2,],
    (dtnsr[1,2,] %o% dtnsr[1,2,])@data)

# tensorrでは行列間のクロス積はできない
# expect_identical(
#     tnsr[1,,]@data %o% tnsr[1,,]@data,
#     (dtnsr[1,,] %o% dtnsr[1,,])@data)

# expect_identical(
#     outer(tnsr[1,2,]@data, tnsr[1,2,]@data, "*"),
#     outer(stnsr[1,2,], stnsr[1,2,], "*"),
#     (outer(dtnsr[1,2,], dtnsr[1,2,], "*"))@data)

# tensorrでは"+"のouterもできない
# expect_identical(
#     outer(tnsr[1,2,]@data, tnsr[1,2,]@data, "+"),
#     (outer(dtnsr[1,2,], dtnsr[1,2,], "+"))@data)

# tensorrでは行列間のクロス積はできない
# expect_identical(
#     outer(tnsr[1,,]@data, tnsr[1,,]@data, "*"),
#     (outer(dtnsr[1,,], dtnsr[1,,], "*"))@data)

# tensorrでは"+"のouterもできない
# expect_identical(
#     outer(tnsr[1,,]@data, tnsr[1,,]@data, "+"),
#     (outer(dtnsr[1,,], dtnsr[1,,], "+"))@data)


context("### 10. x %/% y ###\n")
expect_identical(
    (tnsr %/% 2)@data,
    .sp_to_array(stnsr %/% 2),
    (dtnsr %/% 2)@data)

# tensorrでは%/%が使えない
expect_identical(
    (tnsr %/% tnsr)@data,
    (dtnsr %/% dtnsr)@data)


context("### 11. x %% y ###\n")
expect_identical(
    (tnsr %% 2)@data,
    .sp_to_array(stnsr %% 2),
    (dtnsr %% 2)@data)

# tensorrでは%%が使えない
expect_identical(
    (tnsr %% tnsr)@data,
    (dtnsr %% dtnsr)@data)


context("### 12. sqrt(x) ###\n")
# expect_identical(
#     sqrt(tnsr@data),
#     .sp_to_array(sqrt(stnsr)),
#     sqrt(dtnsr)@data)


context("### 13. abs(x) ###\n")
# expect_identical(
#     abs(tnsr@data),
#     .sp_to_array(abs(stnsr)),
#     abs(dtnsr)@data)


context("### 14. exp(x) ###\n")
# expect_identical(
#     exp(tnsr@data),
#     .sp_to_array(exp(stnsr)),
#     exp(dtnsr)@data)


context("### 15. log(x) ###\n")
# expect_identical(
#     log(tnsr@data),
#     .sp_to_array(log(stnsr)),
#     log(dtnsr)@data)


context("### 16. log10(x) ###\n")
# expect_identical(
#     log10(tnsr@data),
#     .sp_to_array(log10(stnsr)),
#     log10(dtnsr)@data)


context("### 17. log2(x) ###\n")
# expect_identical(
#     log2(tnsr@data),
#     .sp_to_array(log2(stnsr)),
#     log2(dtnsr)@data)


context("### 18. ceiling(x) ###\n")
# expect_identical(
#     ceiling(tnsr@data),
#     .sp_to_array(ceiling(stnsr)),
#     ceiling(dtnsr)@data)


context("### 19. floor(x) ###\n")
# expect_identical(
#     floor(tnsr@data),
#     .sp_to_array(floor(stnsr)),
#     floor(dtnsr)@data)


context("### 20. round(x,digits=0) ###\n")
# expect_identical(
#     round(tnsr@data),
#     .sp_to_array(round(stnsr)),
#     round(dtnsr)@data)
