library("testthat")
library("rTensor")
library("tensorr")
library("openssl")

load("faces_tnsr.RData")
source("sp_to_array.R")
source("DelayedTensor_Arithmetic.R")
source("DelayedTensor_Class.R")
source("DelayedTensor_Decomp.R")
source("DelayedTensor_Misc.R")
source("testdata.R")

options(testthat.use_colours = FALSE)

test_file("test_DelayedTensor_Arithmetic.R")
test_file("test_DelayedTensor_Class.R")
test_file("test_DelayedTensor_Decomp.R")
test_file("test_DelayedTensor_Misc.R")
