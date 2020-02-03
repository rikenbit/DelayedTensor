# tensorrではアルゴリズムは一切実装していない

context("### 44. hosvd ###\n")
hosvdD <- rTensor::hosvd(tnsr)
dhosvdD <- hosvd(dtnsr)

expect_identical(
    hosvdD$Z@data,
    dhosvdD$Z@data)

expect_identical(
    hosvdD$U,
    dhosvdD$U)

expect_identical(
    hosvdD$est@data,
    dhosvdD$est@data)

expect_identical(
    hosvdD$fnorm_resid,
    dhosvdD$fnorm_resid)


context("### 45. cp ###\n")
set.seed(1234)
cpD <- rTensor::cp(tnsr, num_components=10)
set.seed(1234)
dcpD <- cp(dtnsr, num_components=10)

expect_identical(
    cpD$lambdas,
    dcpD$lambdas
)

expect_identical(
    cpD$U,
    dcpD$U
)

expect_identical(
    cpD$conv,
    dcpD$conv
)

expect_identical(
    cpD$est@data,
    dcpD$est@data
)

expect_identical(
    cpD$norm_percent,
    dcpD$norm_percent
)

expect_identical(
    cpD$fnorm_resid,
    dcpD$fnorm_resid
)

expect_identical(
    cpD$all_resid,
    dcpD$all_resid
)


context("### 46. tucker ###\n")
set.seed(1234)
tuckerD <- rTensor::tucker(tnsr, ranks=c(2,3,4))
set.seed(1234)
dtuckerD <- tucker(dtnsr, ranks=c(2,3,4))

expect_identical(
    cpD$Z,
    dcpD$Z
)

expect_identical(
    cpD$U,
    dcpD$U
)

expect_identical(
    cpD$conv,
    dcpD$conv
)

expect_identical(
    cpD$est@data,
    dcpD$est@data
)

expect_identical(
    cpD$norm_percent,
    dcpD$norm_percent
)

expect_identical(
    cpD$fnorm_resid,
    dcpD$fnorm_resid
)

expect_identical(
    cpD$all_resids,
    dcpD$all_resids
)


context("### 47. mpca ###\n")
subject <- faces_tnsr[,,8,]
dsubject <- subject
class(dsubject) <- "DelayedTensor"

options(warn=-1)
set.seed(1234)
mpcaD <- rTensor::mpca(subject, ranks=c(10,10))

set.seed(1234)
dmpcaD <- mpca(dsubject, ranks=c(10,10))

options(warn=1)

expect_identical(
    mpcaD$conv,
    dmpcaD$conv
)

expect_identical(
    mpcaD$norm_percent,
    dmpcaD$norm_percent
)

expect_identical(
    mpcaD$all_resids,
    dmpcaD$all_resids
)


context("### 48. pvd ###\n")
set.seed(1234)
pvdD <- rTensor::pvd(subject, uranks=rep(46,10), wranks=rep(56,10), a=46, b=56)

set.seed(1234)
dpvdD <- pvd(dsubject, uranks=rep(46,10), wranks=rep(56,10), a=46, b=56)

expect_identical(
    pvdD$P,
    dpvdD$P
)

expect_identical(
    pvdD$D,
    dpvdD$D
)

expect_identical(
    pvdD$V,
    dpvdD$V
)

expect_identical(
    pvdD$est@data,
    dpvdD$est@data
)

expect_identical(
    pvdD$norm_percent,
    dpvdD$norm_percent
)

expect_identical(
    pvdD$fnorm_resid,
    dpvdD$fnorm_resid
)


context("### 49. t_svd ###\n")
set.seed(1234)
tsvdD <- rTensor::t_svd(tnsr)
set.seed(1234)
dtsvdD <- t_svd(dtnsr)

expect_identical(
    tsvdD$U@data,
    dtsvdD$U@data
)

expect_identical(
    tsvdD$V@data,
    dtsvdD$V@data
)

expect_identical(
    tsvdD$S@data,
    dtsvdD$S@data
)


context("### 50. t_svd_reconstruct ###\n")
set.seed(1234)
r_tsvdD <- rTensor::t_svd_reconstruct(tsvdD)
set.seed(1234)
r_dtsvdD <- t_svd_reconstruct(dtsvdD)

expect_identical(
    r_tsvdD@num_modes,
    r_dtsvdD@num_modes)

expect_identical(
    r_tsvdD@modes,
    r_dtsvdD@modes)

expect_identical(
    r_tsvdD@data,
    r_dtsvdD@data)


context("### 51. .is_zero_tensor ###\n")
expect_identical(
	rTensor:::.is_zero_tensor(tnsr),
	.is_zero_tensor(dtnsr))
