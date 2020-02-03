library("rTensor")
library("openssl")

load("faces_tnsr.RData")
source("DelayedTensor_Arithmetic.R")
source("DelayedTensor_Class.R")
source("DelayedTensor_Decomp.R")
source("DelayedTensor_Misc.R")

set.seed(1234)
tnsr <- rTensor::rand_tensor()
set.seed(1234)
dtnsr <- rand_tensor()


context("### 21. unfold ###\n")
expect_identical(
    rTensor::unfold(tnsr, row_idx=1, col_idx=c(2, 3))@data,
    unfold(dtnsr, row_idx=1, col_idx=c(2, 3))@data)

expect_identical(
    rTensor::unfold(tnsr, row_idx=2, col_idx=c(3, 1))@data,
    unfold(dtnsr, row_idx=2, col_idx=c(3, 1))@data)

expect_identical(
    rTensor::unfold(tnsr, row_idx=3, col_idx=c(1, 2))@data,
    unfold(dtnsr, row_idx=3, col_idx=c(1, 2))@data)


context("### 22. k_unfold ###\n")
expect_identical(
    rTensor::k_unfold(tnsr, m=1)@data,
    k_unfold(dtnsr, m=1)@data)

expect_identical(
    rTensor::k_unfold(tnsr, m=2)@data,
    k_unfold(dtnsr, m=2)@data)

expect_identical(
    rTensor::k_unfold(tnsr, m=3)@data,
    k_unfold(dtnsr, m=3)@data)


context("### 23. matvec ###\n")
expect_identical(
    rTensor::matvec(tnsr)@data,
    matvec(dtnsr)@data)


context("### 24. rs_unfold ###\n")
expect_identical(
    rTensor::rs_unfold(tnsr, m=1)@data,
    rs_unfold(dtnsr, m=1)@data)

expect_identical(
    rTensor::rs_unfold(tnsr, m=2)@data,
    rs_unfold(dtnsr, m=2)@data)

expect_identical(
    rTensor::rs_unfold(tnsr, m=3)@data,
    rs_unfold(dtnsr, m=3)@data)


context("### 25. cs_unfold ###\n")
expect_identical(
    rTensor::cs_unfold(tnsr, m=1)@data,
    cs_unfold(dtnsr, m=1)@data)

expect_identical(
    rTensor::cs_unfold(tnsr, m=2)@data,
    cs_unfold(dtnsr, m=2)@data)

expect_identical(
    rTensor::cs_unfold(tnsr, m=3)@data,
    cs_unfold(dtnsr, m=3)@data)


context("### 26. modeSum ###\n")
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
    fnorm(dtnsr))


context("### 29. innerProd ###\n")
expect_identical(
    rTensor::innerProd(tnsr, tnsr),
    innerProd(dtnsr, dtnsr))


context("### 30. initialize ###\n")
expect_identical(
    tnsr@num_modes,
    dtnsr@num_modes)

expect_identical(
    tnsr@modes,
    dtnsr@modes)

expect_identical(
    tnsr@data,
    dtnsr@data)


context("### 31. dim ###\n")
expect_identical(
    dim(tnsr),
    dim(dtnsr))

expect_identical(
    dim(tnsr@data),
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
    tnsr[1,2,3]@data,
    dtnsr[1,2,3]@data)

expect_identical(
    tnsr[3,1,]@data,
    dtnsr[3,1,]@data)

expect_identical(
    tnsr[,,5]@data,
    dtnsr[,,5]@data)

expect_identical(
    tnsr[,,5,drop=FALSE]@data,
    dtnsr[,,5,drop=FALSE]@data)


context("### 37. [<- ###\n")
tnsr[1,2,3] <- 3
dtnsr[1,2,3] <- 3

expect_identical(
    tnsr[1,2,3]@data,
    dtnsr[1,2,3]@data)

tnsr[3,1,] <- rep(0,5)
dtnsr[3,1,] <- rep(0,5)

expect_identical(
    tnsr[3,1,]@data,
    dtnsr[3,1,]@data)

tnsr[,2,] <-  matrix(0,nrow=3,ncol=5)
dtnsr[,2,] <-  matrix(0,nrow=3,ncol=5)

expect_identical(
    tnsr[,2,]@data,
    dtnsr[,2,]@data)


context("### 38. t ###\n")
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
matT <- as.delayed.matrix(mat)

expect_identical(
	mat,
	matT@data,
	dmatT@data)


context("### 40. as.delayed.tensor ###\n")
#From vector
set.seed(1234)
vec <- runif(100)
vecT <- rTensor::as.tensor(vec)
dvecT <- as.delayed.tensor(vec)

expect_identical(
	vec,
	as.vector(vecT@data),
	as.vector(dvecT@data))

#From matrix
mat <- matrix(runif(1000), nrow=100, ncol=10)
matT <- rTensor::as.tensor(mat)
dmatT <- as.delayed.tensor(mat)

expect_identical(
	mat,
	matT@data,
	dmatT@data)

#From array
indices <- c(10,20,30,40)
arr <- array(runif(prod(indices)), dim = indices)
arrT <- rTensor::as.tensor(arr)
darrT <- as.delayed.tensor(arr)

expect_identical(
	arr,
	arrT@data,
	darrT@data)


context("### 41. as.tensor ###\n")
# indices <- c(10,20,30,40)
# arr <- array(runif(prod(indices)), dim = indices)
# arrT <- rTensor::as.tensor(arr)
# darrT <- as.tensor(as.delayed.tensor(arr))

# expect_identical(
# 	arrT,
# 	darrT)


context("### 42. tperm ###\n")
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
	vec(dtnsr))

expect_identical(
	rTensor::vec(tnsr),
	as.delayed.vector(dtnsr))