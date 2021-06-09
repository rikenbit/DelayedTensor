library("DelayedArray")
library("HDF5Array")
library("BiocSingular")
library("rTensor")
library("DelayedRandomArray")
library("irlba")
library("Matrix")
library("einsum")
library("DelayedTensor")
library("testthat")

# Setting
options(testthat.use_colours = FALSE)
options(delayedtensor.sparse = FALSE)
options(delayedtensor.verbose = FALSE)
source("testthat/test_Einsum_objects.R")

# source("../R/Decomp.R")
# source("../R/Einsum.R")
# source("../R/Generics.R")
# source("../R/Global-settings.R")
# source("../R/Misc.R")
# source("../R/SVD.R")
# source("../R/Utils.R")
# source("../R/zzz.R")

# for(size in c(1E+9,1E+8,1E+7,1E+6,1E+5,1E+4,1E+3,1E+2,50,20:1)){
# setAutoBlockSize(size=size)
# getAutoBlockSize()
test_file("testthat/test_Generics.R")
test_file("testthat/test_Misc.R")
test_file("testthat/test_SVD.R")
test_file("testthat/test_4thorder.R")
test_file("testthat/test_Einsum.R")
# gc();gc()
# }

# setAutoBlockSize(size=1E+8)
# test_file("testthat/test_Decomp.R")
# test_file("testthat/test_Einsum_Py.R")
# source("testthat/test_Verbose.R")
# source("testthat/test_Sparse.R")

# # Profiling
# load("../data/human_mid_brain.rda")
# load("../data/mouse_mid_brain.rda")
# source("testthat/test_SuperBig.R")
