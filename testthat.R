library("testthat")

options(testthat.use_colours = FALSE)

test_file("test_DelayedTensor_Arithmetic.R")
test_file("test_DelayedTensor_Class.R")
test_file("test_DelayedTensor_Decomp.R")
test_file("test_DelayedTensor_Misc.R")
