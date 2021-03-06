context("### 21. unfold ###\n")
expect_identical(
    rTensor::unfold(tnsr, row_idx=1, col_idx=c(2, 3))@data,
    as.matrix(tensorr::unfold(stnsr, 1)@mat),
    unfold(dtnsr, row_idx=1, col_idx=c(2, 3))@data)

# tensorrのunfoldにはcol_idxオプションがない
# 転置関数t()を作って、どこかのオーダー間だけ入れ替えられればいらないが...
expect_identical(
    rTensor::unfold(tnsr, row_idx=2, col_idx=c(3, 1))@data,
    unfold(dtnsr, row_idx=2, col_idx=c(3, 1))@data)

expect_identical(
    rTensor::unfold(tnsr, row_idx=3, col_idx=c(1, 2))@data,
    as.matrix(tensorr::unfold(stnsr, 3)@mat),
    unfold(dtnsr, row_idx=3, col_idx=c(1, 2))@data)


context("### 22. k_unfold ###\n")
# これがtensorrのunfoldに相当
expect_identical(
    rTensor::k_unfold(tnsr, m=1)@data,
    as.matrix(tensorr::unfold(stnsr, m=1)@mat),
    k_unfold(dtnsr, m=1)@data)

expect_identical(
    rTensor::k_unfold(tnsr, m=2)@data,
    as.matrix(tensorr::unfold(stnsr, m=2)@mat),
    k_unfold(dtnsr, m=2)@data)

expect_identical(
    rTensor::k_unfold(tnsr, m=3)@data,
    as.matrix(tensorr::unfold(stnsr, m=3)@mat),
    k_unfold(dtnsr, m=3)@data)


context("### 23. matvec ###\n")
expect_identical(
    rTensor::matvec(tnsr)@data,
    t(as.matrix(tensorr::unfold(stnsr, m=2)@mat)),
    matvec(dtnsr)@data)


context("### 24. rs_unfold ###\n")
# unfoldした後、横長行列で返す（Short-and-fat）
expect_identical(
    rTensor::rs_unfold(tnsr, m=1)@data,
    as.matrix(tensorr::unfold(stnsr, m=1)@mat),
    rs_unfold(dtnsr, m=1)@data)

expect_identical(
    rTensor::rs_unfold(tnsr, m=2)@data,
    as.matrix(tensorr::unfold(stnsr, m=2)@mat),
    rs_unfold(dtnsr, m=2)@data)

expect_identical(
    rTensor::rs_unfold(tnsr, m=3)@data,
    as.matrix(tensorr::unfold(stnsr, m=3)@mat),
    rs_unfold(dtnsr, m=3)@data)


context("### 25. cs_unfold ###\n")
# unfoldした後、縦長行列で返す（Tall-and-skinny）
expect_identical(
    rTensor::cs_unfold(tnsr, m=1)@data,
    t(as.matrix(tensorr::unfold(stnsr, m=1)@mat)),
    cs_unfold(dtnsr, m=1)@data)

expect_identical(
    rTensor::cs_unfold(tnsr, m=2)@data,
    t(as.matrix(tensorr::unfold(stnsr, m=2)@mat)),
    cs_unfold(dtnsr, m=2)@data)

expect_identical(
    rTensor::cs_unfold(tnsr, m=3)@data,
    t(as.matrix(tensorr::unfold(stnsr, m=3)@mat)),
    cs_unfold(dtnsr, m=3)@data)


context("### 26. modeSum ###\n")
# tensorrには実装がない
expect_identical(
    rTensor::modeSum(tnsr, m=1, drop=FALSE)@data,
    modeSum(dtnsr, m=1, drop=FALSE)@data)

expect_identical(
    rTensor::modeSum(tnsr, m=2, drop=FALSE)@data,
    modeSum(dtnsr, m=2, drop=FALSE)@data)

expect_identical(
    rTensor::modeSum(tnsr, m=3, drop=FALSE)@data,
    modeSum(dtnsr, m=3, drop=FALSE)@data)

expect_identical(
    rTensor::modeSum(tnsr, m=1, drop=TRUE)@data,
    modeSum(dtnsr, m=1, drop=TRUE)@data)

expect_identical(
    rTensor::modeSum(tnsr, m=2, drop=TRUE)@data,
    modeSum(dtnsr, m=2, drop=TRUE)@data)

expect_identical(
    rTensor::modeSum(tnsr, m=3, drop=TRUE)@data,
    modeSum(dtnsr, m=3, drop=TRUE)@data)


context("### 27. modeMean ###\n")
# tensorrには実装がない
expect_identical(
    rTensor::modeMean(tnsr, m=1, drop=FALSE)@data,
    modeMean(dtnsr, m=1, drop=FALSE)@data)

expect_identical(
    rTensor::modeMean(tnsr, m=2, drop=FALSE)@data,
    modeMean(dtnsr, m=2, drop=FALSE)@data)

expect_identical(
    rTensor::modeMean(tnsr, m=3, drop=FALSE)@data,
    modeMean(dtnsr, m=3, drop=FALSE)@data)

expect_identical(
    rTensor::modeMean(tnsr, m=1, drop=TRUE)@data,
    modeMean(dtnsr, m=1, drop=TRUE)@data)

expect_identical(
    rTensor::modeMean(tnsr, m=2, drop=TRUE)@data,
    modeMean(dtnsr, m=2, drop=TRUE)@data)

expect_identical(
    rTensor::modeMean(tnsr, m=3, drop=TRUE)@data,
    modeMean(dtnsr, m=3, drop=TRUE)@data)


context("### 28. fnorm ###\n")
expect_identical(
    rTensor::fnorm(tnsr),
    tensorr::norm(stnsr),
    fnorm(dtnsr))


context("### 29. innerProd ###\n")
expect_identical(
    rTensor::innerProd(tnsr, tnsr),
    tensorr::innerprod(stnsr, stnsr),
    innerProd(dtnsr, dtnsr))


context("### 30. initialize ###\n")
expect_identical(
    tnsr@num_modes,
    length(stnsr@dims),
    dtnsr@num_modes)

expect_identical(
    tnsr@modes,
    stnsr@dims,
    dtnsr@modes)

expect_identical(
    tnsr@data,
    .sp_to_array(stnsr),
    dtnsr@data)


context("### 31. dim ###\n")
# tensorrでは実装がない
expect_identical(
    dim(tnsr),
    stnsr@dims,
    dim(dtnsr))

expect_identical(
    dim(tnsr@data),
    dim(.sp_to_array(stnsr)),
    dim(dtnsr@data))


context("### 32. show ###\n")
d <- tempdir()

file1 <- paste0(d, "/show_rTensor")
sink(file1)
rTensor::show(tnsr)
sink()

file2 <- paste0(d, "/show_DelayedTensor")
sink(file2)
show(dtnsr)
sink()

expect_identical(
    md5(file(file1)),
    md5(file(file2)))


context("### 33. print ###\n")
d <- tempdir()

file1 <- paste0(d, "/print_rTensor")
sink(file1)
rTensor::print(tnsr)
sink()

file2 <- paste0(d, "/print_DelayedTensor")
sink(file2)
print(dtnsr)
sink()

expect_identical(
    md5(file(file1)),
    md5(file(file2)))


context("### 34. head ###\n")
d <- tempdir()

file1 <- paste0(d, "/head_rTensor")
sink(file1)
rTensor::head(tnsr)
sink()

file2 <- paste0(d, "/head_DelayedTensor")
sink(file2)
head(dtnsr)
sink()

file3 <- paste0(d, "/head_data_rTensor")
sink(file3)
rTensor::head(tnsr@data)
sink()

file4 <- paste0(d, "/head_data_DelayedTensor")
sink(file4)
head(dtnsr@data)
sink()

expect_identical(
    md5(file(file1)),
    md5(file(file2)),
    md5(file(file3)),
    md5(file(file4)))


context("### 35. tail ###\n")
d <- tempdir()

file1 <- paste0(d, "/tail_rTensor")
sink(file1)
rTensor::tail(tnsr)
sink()

file2 <- paste0(d, "/tail_DelayedTensor")
sink(file2)
tail(dtnsr)
sink()

file3 <- paste0(d, "/tail_data_rTensor")
sink(file3)
rTensor::tail(tnsr@data)
sink()

file4 <- paste0(d, "/tail_data_DelayedTensor")
sink(file4)
tail(dtnsr@data)
sink()

expect_identical(
    md5(file(file1)),
    md5(file(file2)),
    md5(file(file3)),
    md5(file(file4)))


context("### 36. [ ###\n")
expect_identical(
    tnsr@data[1,2,3],
    stnsr[1,2,3],
    dtnsr@data[1,2,3])

expect_identical(
    tnsr@data[3,1,],
    .sp_to_array(stnsr[3,1,]),
    dtnsr@data[3,1,])

expect_identical(
    tnsr@data[,,5],
    .sp_to_array(stnsr[,,5])[,,1], # tensorr的drop=FALSEの動作
    dtnsr@data[,,5])

expect_identical(
    tnsr[,,5, drop=FALSE]@data,
    .sp_to_array(stnsr[,,5]),
    dtnsr[,,5, drop=FALSE]@data)


context("### 37. [<- ###\n")
tnsr[1,2,3] <- 3
stnsr[1,2,3] <- 3
dtnsr[1,2,3] <- 3

expect_identical(
    tnsr@data[1,2,3],
    stnsr[1,2,3],
    dtnsr@data[1,2,3])

tnsr[3,1,] <- rep(0,5)
stnsr[3,1,] <- rep(0,5)
dtnsr[3,1,] <- rep(0,5)

expect_identical(
    tnsr@data[3,1,],
    .sp_to_array(stnsr[3,1,]),
    dtnsr@data[3,1,])

tnsr[,2,] <-  matrix(0,nrow=3,ncol=5)
stnsr[,2,] <-  matrix(0,nrow=3,ncol=5)
dtnsr[,2,] <-  matrix(0,nrow=3,ncol=5)

expect_identical(
    tnsr@data[,2,],
    .sp_to_array(stnsr[,2,])[,1,],
    dtnsr@data[,2,])


context("### 38. t ###\n")
# tensorrにはない
expect_identical(
	rTensor::t(tnsr)@data[,,1],
	rTensor::t(tnsr@data[,,1]))

expect_identical(
	t(tnsr)@data[,,2],
	t(tnsr@data[,,5]))

expect_identical(
	rTensor::t(rTensor::t(tnsr)),
	tnsr)



expect_identical(
	t(dtnsr)@data[,,1],
	t(dtnsr@data[,,1]))

expect_identical(
	t(dtnsr)@data[,,2],
	t(dtnsr@data[,,5]))

expect_identical(
	t(t(dtnsr)),
	dtnsr)



expect_identical(
	rTensor::t(tnsr)@data[,,1],
	t(dtnsr@data[,,1]))

expect_identical(
	rTensor::t(tnsr)@data[,,2],
	t(dtnsr@data[,,5]))

expect_identical(
	rTensor::t(rTensor::t(tnsr))@data,
	dtnsr@data)


context("### 39. as.delayed.matrix ###\n")
#From matrix
mat <- matrix(runif(1000), nrow=100, ncol=10)
matT <- rTensor::as.tensor(mat)
smatT <- tensorr::as_sptensor(tensorr::as_dtensor(mat))
dmatT <- as.delayed.matrix(mat)

expect_identical(
	mat,
	matT@data,
    .sp_to_array(smatT),
	dmatT@data)


context("### 40. as.delayed.tensor ###\n")
#From vector
set.seed(1234)
vec <- runif(100)
vecT <- rTensor::as.tensor(vec)
svecT <- tensorr::as_sptensor(tensorr::as_dtensor(as.matrix(vec)))
dvecT <- as.delayed.tensor(vec)

expect_identical(
	vec,
	as.vector(vecT@data),
    as.vector(smatT),
	as.vector(dvecT@data))

#From array
indices <- c(10,20,30,40)
arr <- array(runif(prod(indices)), dim = indices)
arrT <- rTensor::as.tensor(arr)
sarrT <- tensorr::as_sptensor(tensorr::as_dtensor(arr))
darrT <- as.delayed.tensor(arr)

expect_identical(
	arr,
	arrT@data,
    .sp_to_array(sarrT),
	darrT@data)


context("### 41. as.tensor ###\n")
# darrT <- as.tensor(as.delayed.tensor(arr))

# expect_identical(
#     arr,
#     arrT@data,
#     .sp_to_array(sarrT),
#     darrT@data)


context("### 42. tperm ###\n")
# tensorrには無い
A <- rTensor::tperm(tnsr, perm=c(2,1,3))
B <- tperm(dtnsr, perm=c(2,1,3))

expect_identical(
	A@num_modes,
	B@num_modes)

expect_identical(
	A@modes,
	B@modes)

expect_identical(
	A@data,
	B@data)

context("### 43. vec / as.delayed.vector ###\n")
expect_identical(
	rTensor::vec(tnsr),
    as.vector(stnsr),
	vec(dtnsr))

expect_identical(
	rTensor::vec(tnsr),
    as.vector(stnsr),
	as.delayed.vector(dtnsr))
