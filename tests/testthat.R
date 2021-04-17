library("testthat")
library("rTensor")

options(testthat.use_colours = FALSE)

test_file("testthat/test_AllGenerics.R")
test_file("testthat/test_Decomp.R")
test_file("testthat/test_Misc.R")
