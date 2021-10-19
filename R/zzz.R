.onLoad <- function(libname, pkgname){
    options(delayedtensor.sparse = FALSE)
    options(delayedtensor.verbose = FALSE)
    setHDF5DumpCompressionLevel(level=0L)
}
