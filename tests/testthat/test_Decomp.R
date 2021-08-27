# Setting
# arr <- array(runif(3*4*5), dim=c(3,4,5))
# tnsr <- as.tensor(arr)
# darr <- DelayedArray(arr)
faces_tnsr <- load_orl()
subject <- faces_tnsr[,,21,]
dsubject <- DelayedArray(faces_tnsr[,,21,]@data)
# faces_tnsr <- array(runif(10*20*30*10), dim=c(10,20,30,10))
# subject <- as.tensor(faces_tnsr[,,21,])
# dsubject <- DelayedArray(faces_tnsr[,,21,])

# # visual confirmation
# png(file="GrandTruth.png")
# image(subject@data[,,1])
# dev.off()

context("### hosvd ###\n")
set.seed(1234)
hosvdD <- rTensor::hosvd(subject, ranks=c(10,10,10))
set.seed(1234)
dhosvdD <- hosvd(dsubject, ranks=c(10,10,10))

expect_equal(
    dim(hosvdD$Z@data),
    dim(dhosvdD$Z))
expect_equal(
    lapply(hosvdD$U, dim),
    lapply(dhosvdD$U, dim))
expect_equal(
    dim(hosvdD$est@data),
    dim(dhosvdD$est))
expect_true(abs(hosvdD$fnorm_resid - dhosvdD$fnorm_resid) <= 0.1)

# # visual confirmation
# png(file="HOSVD.png")
# image(hosvdD$est@data[,,1])
# dev.off()
# png(file="HOSVD_DelayedTensor.png")
# image(as.array(dhosvdD$est)[,,1])
# dev.off()

context("### cp ###\n")
set.seed(1234)
cpD <- rTensor::cp(subject, num_components=10, max_iter=20)
set.seed(1234)
dcpD <- cp(dsubject, num_components=10, max_iter=20)

expect_equal(
    length(cpD$lambdas),
    length(dcpD$lambdas))
expect_equal(
    lapply(cpD$U, dim),
    lapply(dcpD$U, dim))
expect_equal(
    dim(cpD$est),
    dim(dcpD$est))
expect_equal(
    cpD$norm_percent >= 0,
    dcpD$norm_percent >= 0)
l <- min(length(cpD$all_resid), length(dcpD$all_resid))
expect_equal(
    order(cpD$all_resid[seq(l)]),
    order(dcpD$all_resid[seq(l)]))
expect_true(abs(cpD$fnorm_resid - dcpD$fnorm_resid) <= 1)

# # visual confirmation
# core <- array(0, dim=rep(length(cpD$lambdas), 3))
# for(i in seq_along(cpD$lambdas)){
#     core[i,i,i] <- cpD$lambdas[i]
# }
# dcore <- DelayedArray(core)
# png(file="CP.png")
# image(cpD$est@data[,,1])
# dev.off()
# png(file="CP_DelayedTensor.png")
# image(as.array(dcpD$est)[,,1])
# dev.off()

context("### tucker ###\n")
set.seed(1234)
tuckerD <- rTensor::tucker(subject, ranks=c(10,10,10), max_iter=20)
set.seed(1234)
dtuckerD <- tucker(dsubject, ranks=c(10,10,10), max_iter=20)
expect_equal(
    dim(tuckerD$Z),
    dim(dtuckerD$Z)
)
expect_equal(
    lapply(tuckerD$U, dim),
    lapply(dtuckerD$U, dim)
)
expect_equal(
    tuckerD$conv,
    dtuckerD$conv
)
expect_equal(
    dim(tuckerD$est),
    dim(dtuckerD$est)
)
expect_equal(
    tuckerD$norm_percent >= 0,
    dtuckerD$norm_percent >= 0
)
l <- min(length(tuckerD$all_resid), length(dtuckerD$all_resid))
expect_equal(
    order(tuckerD$all_resid[seq(l)]),
    order(dtuckerD$all_resid[seq(l)]))
expect_true(abs(tuckerD$fnorm_resid - dtuckerD$fnorm_resid) <= 0.1)

# # visual confirmation
# png(file="TUCKER.png")
# image(tuckerD$est@data[,,1])
# dev.off()
# png(file="TUCKER_DelayedTensor.png")
# image(as.array(dtuckerD$est)[,,1])
# dev.off()

context("### mpca ###\n")
options(warn=-1)
set.seed(1234)
mpcaD <- rTensor::mpca(subject, ranks=c(10,10), max_iter=20)
set.seed(1234)
dmpcaD <- mpca(dsubject, ranks=c(10,10), max_iter=20)
options(warn=1)
expect_equal(
    mpcaD$conv,
    dmpcaD$conv
)
expect_equal(
    mpcaD$norm_percent >= 0,
    dmpcaD$norm_percent >= 0
)
l <- min(length(mpcaD$all_resid), length(dmpcaD$all_resid))
expect_equal(
    order(mpcaD$all_resid[seq(l)]),
    order(dmpcaD$all_resid[seq(l)]))
expect_true(abs(mpcaD$fnorm_resid - dmpcaD$fnorm_resid) <= 0.1)

# # visual confirmation
# png(file="MPCA.png")
# image(rTensor::ttl(mpcaD$Z_ext, mpcaD$U[1:2], ms=1:2)@data[,,1])
# dev.off()
# png(file="MPCA_DelayedTensor.png")
# image(as.array(ttl(dmpcaD$Z_ext, dmpcaD$U[1:2], ms=1:2))[,,1])
# dev.off()

context("### pvd ###\n")
set.seed(1234)
pvdD <- rTensor::pvd(subject, uranks=rep(5,10), wranks=rep(8,10), a=5, b=8)
set.seed(1234)
dpvdD <- pvd(dsubject, uranks=rep(5,10), wranks=rep(8,10), a=5, b=8)
expect_equal(
    dim(pvdD$P),
    dim(dpvdD$P)
)
expect_equal(
    dim(pvdD$D),
    dim(dpvdD$D)
)
expect_equal(
    dim(pvdD$V),
    dim(dpvdD$V)
)
expect_equal(
    dim(pvdD$est),
    dim(dpvdD$est)
)
expect_equal(
    pvdD$norm_percent >= 0,
    dpvdD$norm_percent >= 0
)
expect_true(abs(pvdD$fnorm_resid - dpvdD$fnorm_resid) <= 1)

# # visual confirmation
# png(file="PVD.png")
# image(pvdD$P %*% pvdD$V[[1]] %*% pvdD$D)
# dev.off()
# png(file="PVD_DelayedTensor.png")
# image(as.array(dpvdD$P %*% dpvdD$V[[1]] %*% dpvdD$D))
# dev.off()

context("### .is_zero_tensor ###\n")
expect_equal(
	rTensor:::.is_zero_tensor(tnsr),
	.is_zero_tensor(darr))
