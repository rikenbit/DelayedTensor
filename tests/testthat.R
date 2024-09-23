library("DelayedArray")
# library("HDF5Array") # comment out
# library("BiocSingular") # comment out
library("rTensor")
library("DelayedRandomArray")
# library("irlba") # comment out
# library("Matrix") # comment out
library("einsum")
library("DelayedTensor")
library("reticulate")
library("testthat")
library("HDF5Array")

# Setting
options(testthat.use_colours = FALSE)
options(delayedtensor.sparse = FALSE)
options(delayedtensor.verbose = FALSE)
setHDF5DumpChunkLength(length = 1000000L)
setHDF5DumpCompressionLevel(level = 9L)
source("testthat/test_Einsum_objects.R")

# source("../R/Decomp.R") # comment out
# source("../R/Einsum.R") # comment out
# source("../R/Generics.R") # comment out
# source("../R/Global-settings.R") # comment out
# source("../R/Misc.R") # comment out
# source("../R/SVD.R") # comment out
# source("../R/Utils.R") # comment out
# source("../R/zzz.R") # comment out

# for(size in c(1E+9,1E+8,1E+7,1E+6,1E+5,1E+4,1E+3,1E+2,50,20:1)){ # comment out
# setAutoBlockSize(size=size) # comment out
# getAutoBlockSize() # comment out
test_file("testthat/test_Generics.R")
test_file("testthat/test_Misc.R")
# test_file("testthat/test_SVD.R") # comment out
test_file("testthat/test_4thorder.R")
test_file("testthat/test_Einsum.R")
# gc();gc() # comment out
# } # comment out

# setAutoBlockSize(size=1E+8)
# test_file("testthat/test_Decomp.R")
# test_file("testthat/test_Einsum_Py.R") # comment out
# source("testthat/test_Verbose.R")
# source("testthat/test_Sparse.R")

# Profiling
# source("testthat/test_SuperBig.R") # comment out
