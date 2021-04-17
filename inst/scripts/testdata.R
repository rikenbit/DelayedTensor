set.seed(1234)
tnsr <- rTensor::rand_tensor()
tnsr <- tnsr * tnsr

set.seed(1234)
idx_x <- sample(dim(tnsr)[1], dim(tnsr)[1]*0.7)
idx_y <- sample(dim(tnsr)[2], dim(tnsr)[2]*0.7)
idx_z <- sample(dim(tnsr)[3], dim(tnsr)[3]*0.7)
tnsr[idx_x, idx_y, idx_z] <- 0

stnsr <- as_sptensor(as_dtensor(tnsr@data))

set.seed(1234)
dtnsr <- rand_tensor()
dtnsr <- dtnsr * dtnsr
dtnsr[idx_x, idx_y, idx_z] <- 0
