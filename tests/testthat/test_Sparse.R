# Setting
setAutoBlockSize(size=1E+8)
options(delayedtensor.sparse = TRUE)

test_file("testthat/test_Generics.R")
test_file("testthat/test_Misc.R")
test_file("testthat/test_Decomp.R")
test_file("testthat/test_4thorder.R")
test_file("testthat/test_Einsum.R")

options(delayedtensor.sparse = FALSE)
