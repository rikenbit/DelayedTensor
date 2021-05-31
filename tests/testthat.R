library("DelayedArray")
library("HDF5Array")
library("BiocSingular")
library("testthat")
library("rTensor")
library("DelayedRandomArray")
library("einsum")

options(testthat.use_colours = FALSE)

# for(size in c(1E+9,1E+8,1E+7,1E+6,1E+5,1E+4,1E+3,1E+2,50,20:1)){
# setAutoBlockSize(size=size)
# getAutoBlockSize()
test_file("testthat/test_AllGenerics.R")
test_file("testthat/test_Misc.R")
test_file("testthat/test_SVD.R")
test_file("testthat/test_Decomp.R")
test_file("testthat/test_4thorder.R")
test_file("testthat/test_Einsum_objects.R")
test_file("testthat/test_Einsum.R")
# gc();gc()
# }
