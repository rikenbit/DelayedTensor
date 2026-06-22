.onLoad <- function(libname, pkgname){
    options(delayedtensor.sparse = FALSE) # Suppressing Experimental Mode
    options(delayedtensor.verbose = FALSE)
    options(delayedtensor.backend = "HDF5Array")
    setHDF5DumpChunkLength(length = 100000L)
    setHDF5DumpCompressionLevel(level = 0L)
}
