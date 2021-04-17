library("testthat")
library("rTensor")

options(testthat.use_colours = FALSE)

test_file("testthat/test_DelayedTensor_Class.R")
test_file("testthat/test_DelayedTensor_Decomp.R")
test_file("testthat/test_DelayedTensor_Misc.R")
