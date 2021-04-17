context("### 21. unfold ###\n")
# expect_identical(
#     rTensor::unfold(tnsr, row_idx=1, col_idx=c(2, 3))@data,
#     unfold(dtnsr, row_idx=1, col_idx=c(2, 3))@data)

# # tensorrのunfoldにはcol_idxオプションがない
# # 転置関数t()を作って、どこかのオーダー間だけ入れ替えられればいらないが...
# expect_identical(
#     rTensor::unfold(tnsr, row_idx=2, col_idx=c(3, 1))@data,
#     unfold(dtnsr, row_idx=2, col_idx=c(3, 1))@data)

# expect_identical(
#     rTensor::unfold(tnsr, row_idx=3, col_idx=c(1, 2))@data,
#     unfold(dtnsr, row_idx=3, col_idx=c(1, 2))@data)


context("### 22. k_unfold ###\n")
# # これがtensorrのunfoldに相当
# expect_identical(
#     rTensor::k_unfold(tnsr, m=1)@data,
#     k_unfold(dtnsr, m=1)@data)

# expect_identical(
#     rTensor::k_unfold(tnsr, m=2)@data,
#     k_unfold(dtnsr, m=2)@data)

# expect_identical(
#     rTensor::k_unfold(tnsr, m=3)@data,
#     k_unfold(dtnsr, m=3)@data)


context("### 23. matvec ###\n")
# expect_identical(
#     rTensor::matvec(tnsr)@data,
#     matvec(dtnsr)@data)


context("### 24. rs_unfold ###\n")
# # unfoldした後、横長行列で返す（Short-and-fat）
# expect_identical(
#     rTensor::rs_unfold(tnsr, m=1)@data,
#     rs_unfold(dtnsr, m=1)@data)

# expect_identical(
#     rTensor::rs_unfold(tnsr, m=2)@data,
#     rs_unfold(dtnsr, m=2)@data)

# expect_identical(
#     rTensor::rs_unfold(tnsr, m=3)@data,
#     rs_unfold(dtnsr, m=3)@data)


context("### 25. cs_unfold ###\n")
# # unfoldした後、縦長行列で返す（Tall-and-skinny）
# expect_identical(
#     rTensor::cs_unfold(tnsr, m=1)@data,
#     cs_unfold(dtnsr, m=1)@data)

# expect_identical(
#     rTensor::cs_unfold(tnsr, m=2)@data,
#     cs_unfold(dtnsr, m=2)@data)

# expect_identical(
#     rTensor::cs_unfold(tnsr, m=3)@data,
#     cs_unfold(dtnsr, m=3)@data)


context("### 26. modeSum ###\n")
# expect_identical(
#     rTensor::modeSum(tnsr, m=1, drop=FALSE)@data,
#     modeSum(dtnsr, m=1, drop=FALSE)@data)

# expect_identical(
#     rTensor::modeSum(tnsr, m=2, drop=FALSE)@data,
#     modeSum(dtnsr, m=2, drop=FALSE)@data)

# expect_identical(
#     rTensor::modeSum(tnsr, m=3, drop=FALSE)@data,
#     modeSum(dtnsr, m=3, drop=FALSE)@data)

# expect_identical(
#     rTensor::modeSum(tnsr, m=1, drop=TRUE)@data,
#     modeSum(dtnsr, m=1, drop=TRUE)@data)

# expect_identical(
#     rTensor::modeSum(tnsr, m=2, drop=TRUE)@data,
#     modeSum(dtnsr, m=2, drop=TRUE)@data)

# expect_identical(
#     rTensor::modeSum(tnsr, m=3, drop=TRUE)@data,
#     modeSum(dtnsr, m=3, drop=TRUE)@data)


context("### 27. modeMean ###\n")
# expect_identical(
#     rTensor::modeMean(tnsr, m=1, drop=FALSE)@data,
#     modeMean(dtnsr, m=1, drop=FALSE)@data)

# expect_identical(
#     rTensor::modeMean(tnsr, m=2, drop=FALSE)@data,
#     modeMean(dtnsr, m=2, drop=FALSE)@data)

# expect_identical(
#     rTensor::modeMean(tnsr, m=3, drop=FALSE)@data,
#     modeMean(dtnsr, m=3, drop=FALSE)@data)

# expect_identical(
#     rTensor::modeMean(tnsr, m=1, drop=TRUE)@data,
#     modeMean(dtnsr, m=1, drop=TRUE)@data)

# expect_identical(
#     rTensor::modeMean(tnsr, m=2, drop=TRUE)@data,
#     modeMean(dtnsr, m=2, drop=TRUE)@data)

# expect_identical(
#     rTensor::modeMean(tnsr, m=3, drop=TRUE)@data,
#     modeMean(dtnsr, m=3, drop=TRUE)@data)


context("### 28. fnorm ###\n")
# expect_identical(
#     rTensor::fnorm(tnsr),
#     fnorm(dtnsr))


context("### 29. innerProd ###\n")
# expect_identical(
#     rTensor::innerProd(tnsr, tnsr),
#     innerProd(dtnsr, dtnsr))


context("### 30. initialize ###\n")
# expect_identical(
#     tnsr@num_modes,
#     dtnsr@num_modes)

# expect_identical(
#     tnsr@modes,
#     dtnsr@modes)

# expect_identical(
#     tnsr@data,
#     dtnsr@data)


context("### 31. dim ###\n")
# expect_identical(
#     dim(tnsr),
#     dim(dtnsr))

# expect_identical(
#     dim(tnsr@data),
#     dim(dtnsr@data))


context("### 32. show ###\n")
# d <- tempdir()

# file1 <- paste0(d, "/show_rTensor")
# sink(file1)
# rTensor::show(tnsr)
# sink()

# file2 <- paste0(d, "/show_DelayedTensor")
# sink(file2)
# show(dtnsr)
# sink()

# expect_identical(
#     md5(file(file1)),
#     md5(file(file2)))


context("### 33. print ###\n")
# d <- tempdir()

# file1 <- paste0(d, "/print_rTensor")
# sink(file1)
# rTensor::print(tnsr)
# sink()

# file2 <- paste0(d, "/print_DelayedTensor")
# sink(file2)
# print(dtnsr)
# sink()

# expect_identical(
#     md5(file(file1)),
#     md5(file(file2)))


context("### 34. head ###\n")
# d <- tempdir()

# file1 <- paste0(d, "/head_rTensor")
# sink(file1)
# rTensor::head(tnsr)
# sink()

# file2 <- paste0(d, "/head_DelayedTensor")
# sink(file2)
# head(dtnsr)
# sink()

# file3 <- paste0(d, "/head_data_rTensor")
# sink(file3)
# rTensor::head(tnsr@data)
# sink()

# file4 <- paste0(d, "/head_data_DelayedTensor")
# sink(file4)
# head(dtnsr@data)
# sink()

# expect_identical(
#     md5(file(file1)),
#     md5(file(file2)),
#     md5(file(file3)),
#     md5(file(file4)))


context("### 35. tail ###\n")
# d <- tempdir()

# file1 <- paste0(d, "/tail_rTensor")
# sink(file1)
# rTensor::tail(tnsr)
# sink()

# file2 <- paste0(d, "/tail_DelayedTensor")
# sink(file2)
# tail(dtnsr)
# sink()

# file3 <- paste0(d, "/tail_data_rTensor")
# sink(file3)
# rTensor::tail(tnsr@data)
# sink()

# file4 <- paste0(d, "/tail_data_DelayedTensor")
# sink(file4)
# tail(dtnsr@data)
# sink()

# expect_identical(
#     md5(file(file1)),
#     md5(file(file2)),
#     md5(file(file3)),
#     md5(file(file4)))


context("### 36. [ ###\n")
# expect_identical(
#     tnsr@data[1,2,3],
#     dtnsr@data[1,2,3])

# expect_identical(
#     tnsr@data[3,1,],
#     dtnsr@data[3,1,])

# expect_identical(
#     tnsr@data[,,5],
#     dtnsr@data[,,5])

# expect_identical(
#     tnsr[,,5, drop=FALSE]@data,
#     dtnsr[,,5, drop=FALSE]@data)


context("### 37. [<- ###\n")
# tnsr[1,2,3] <- 3
# dtnsr[1,2,3] <- 3

# expect_identical(
#     tnsr@data[1,2,3],
#     dtnsr@data[1,2,3])

# tnsr[3,1,] <- rep(0,5)
# dtnsr[3,1,] <- rep(0,5)

# expect_identical(
#     tnsr@data[3,1,],
#     dtnsr@data[3,1,])

# tnsr[,2,] <-  matrix(0,nrow=3,ncol=5)
# dtnsr[,2,] <-  matrix(0,nrow=3,ncol=5)

# expect_identical(
#     tnsr@data[,2,],
#     dtnsr@data[,2,])


context("### 38. t ###\n")
# # tensorrにはない
# expect_identical(
# 	rTensor::t(tnsr)@data[,,1],
# 	rTensor::t(tnsr@data[,,1]))

# expect_identical(
# 	t(tnsr)@data[,,2],
# 	t(tnsr@data[,,5]))

# expect_identical(
# 	rTensor::t(rTensor::t(tnsr)),
# 	tnsr)



# expect_identical(
# 	t(dtnsr)@data[,,1],
# 	t(dtnsr@data[,,1]))

# expect_identical(
# 	t(dtnsr)@data[,,2],
# 	t(dtnsr@data[,,5]))

# expect_identical(
# 	t(t(dtnsr)),
# 	dtnsr)



# expect_identical(
# 	rTensor::t(tnsr)@data[,,1],
# 	t(dtnsr@data[,,1]))

# expect_identical(
# 	rTensor::t(tnsr)@data[,,2],
# 	t(dtnsr@data[,,5]))

# expect_identical(
# 	rTensor::t(rTensor::t(tnsr))@data,
# 	dtnsr@data)

context("### 41. as.tensor ###\n")
# # darrT <- as.tensor(as.delayed.tensor(arr))

# # expect_identical(
# #     arr,
# #     arrT@data,
# #     .sp_to_array(sarrT),
# #     darrT@data)


context("### 42. tperm ###\n")
# A <- rTensor::tperm(tnsr, perm=c(2,1,3))
# B <- tperm(dtnsr, perm=c(2,1,3))

# expect_identical(
# 	A@num_modes,
# 	B@num_modes)

# expect_identical(
# 	A@modes,
# 	B@modes)

# expect_identical(
# 	A@data,
# 	B@data)

context("### 43. vec ###\n")
# expect_identical(
# 	rTensor::vec(tnsr),
# 	vec(dtnsr))