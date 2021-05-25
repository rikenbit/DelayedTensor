context("### .sarray ###\n")
expect_equal(
    dim(.sarray(c(3,4,5))),
    c(3,4,5))

context("### .darray ###\n")
expect_equal(
    dim(.darray(c(3,4,5))),
    c(3,4,5))
