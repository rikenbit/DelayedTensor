# Setting
arr <- array(runif(2*3*4), dim=c(2,3,4))
tnsr <- as.tensor(arr)
darr <- DelayedArray(arr)
dummy_faces_tnsr <- array(runif(10*20*30*10), dim=c(10,20,30,10))
subject <- as.tensor(dummy_faces_tnsr[,,21,])
dsubject <- DelayedArray(dummy_faces_tnsr[,,21,])

context("### hosvd ###\n")
set.seed(1234)
hosvdD <- rTensor::hosvd(tnsr, ranks=c(1,2,3))
set.seed(1234)
dhosvdD <- hosvd(darr, ranks=c(1,2,3))
expect_identical(
    hosvdD$Z@data,
    as.array(dhosvdD$Z))
expect_identical(
    hosvdD$U,
    lapply(dhosvdD$U, as.array))
expect_identical(
    hosvdD$est@data,
    as.array(dhosvdD$est))
expect_true(abs(hosvdD$fnorm_resid - dhosvdD$fnorm_resid) <= 0.1)

context("### cp ###\n")
set.seed(1234)
cpD <- rTensor::cp(tnsr, num_components=2, max_iter=2)
set.seed(1234)
dcpD <- cp(darr, num_components=2, max_iter=2)
expect_identical(
    length(cpD$lambdas),
    length(dcpD$lambdas))
expect_identical(
    lapply(cpD$U, dim),
    lapply(dcpD$U, dim))
expect_identical(
    dim(cpD$est),
    dim(dcpD$est))
expect_identical(
    cpD$norm_percent >= 0,
    dcpD$norm_percent >= 0)
l <- min(length(cpD$all_resid), length(dcpD$all_resid))
expect_identical(
    order(cpD$all_resid[seq(l)]),
    order(dcpD$all_resid[seq(l)]))
expect_true(abs(cpD$fnorm_resid - dcpD$fnorm_resid) <= 0.1)

context("### tucker ###\n")
set.seed(1234)
tuckerD <- rTensor::tucker(tnsr, ranks=c(2,3,4), max_iter=2)
set.seed(1234)
dtuckerD <- tucker(darr, ranks=c(2,3,4), max_iter=2)
expect_identical(
    dim(tuckerD$Z),
    dim(dtuckerD$Z)
)
expect_identical(
    lapply(tuckerD$U, dim),
    lapply(dtuckerD$U, dim)
)
expect_identical(
    tuckerD$conv,
    dtuckerD$conv
)
expect_identical(
    dim(tuckerD$est),
    dim(dtuckerD$est)
)
expect_identical(
    tuckerD$norm_percent >= 0,
    dtuckerD$norm_percent >= 0
)
l <- min(length(tuckerD$all_resid), length(dtuckerD$all_resid))
expect_identical(
    order(tuckerD$all_resid[seq(l)]),
    order(dtuckerD$all_resid[seq(l)]))
expect_true(abs(tuckerD$fnorm_resid - dtuckerD$fnorm_resid) <= 0.1)

context("### mpca ###\n")
options(warn=-1)
set.seed(1234)
mpcaD <- rTensor::mpca(subject, ranks=c(10,10), max_iter=2)
set.seed(1234)
dmpcaD <- mpca(dsubject, ranks=c(10,10), max_iter=2)
options(warn=1)
expect_identical(
    mpcaD$conv,
    dmpcaD$conv
)
expect_identical(
    mpcaD$norm_percent >= 0,
    dmpcaD$norm_percent >= 0
)
l <- min(length(mpcaD$all_resid), length(dmpcaD$all_resid))
expect_identical(
    order(mpcaD$all_resid[seq(l)]),
    order(dmpcaD$all_resid[seq(l)]))
expect_true(abs(mpcaD$fnorm_resid - dmpcaD$fnorm_resid) <= 0.1)


context("### pvd ###\n")
set.seed(1234)
pvdD <- rTensor::pvd(subject, uranks=rep(5,10), wranks=rep(8,10), a=5, b=8)
set.seed(1234)
dpvdD <- pvd(dsubject, uranks=rep(5,10), wranks=rep(8,10), a=5, b=8)
expect_identical(
    dim(pvdD$P),
    dim(dpvdD$P)
)
expect_identical(
    dim(pvdD$D),
    dim(dpvdD$D)
)
expect_identical(
    dim(pvdD$V),
    dim(dpvdD$V)
)
expect_identical(
    dim(pvdD$est),
    dim(dpvdD$est)
)
expect_identical(
    pvdD$norm_percent >= 0,
    dpvdD$norm_percent >= 0
)
expect_true(abs(pvdD$fnorm_resid - dpvdD$fnorm_resid) <= 0.1)

context("### .is_zero_tensor ###\n")
expect_identical(
	rTensor:::.is_zero_tensor(tnsr),
	.is_zero_tensor(darr))
