# Setting
setAutoBlockSize(size=1E+8)
options(delayedtensor.verbose = TRUE)

test_file("testthat/test_Generics.R")

options(delayedtensor.verbose = FALSE)
