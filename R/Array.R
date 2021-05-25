#
# Intermediate array objects
#
.sarray <- function(dim){
	setAutoRealizationBackend("HDF5Array")
	dim <- as.integer(dim)
	sink <- AutoRealizationSink(dim, as.sparse=TRUE)
	close(sink)
	M <- as(sink, "DelayedArray")
	M
}

.darray <- function(dim){
	setAutoRealizationBackend("HDF5Array")
	dim <- as.integer(dim)
	sink <- AutoRealizationSink(dim, as.sparse=FALSE)
	close(sink)
	M <- as(sink, "DelayedArray")
	M
}

# ref
# https://rdrr.io/bioc/DelayedArray/man/write_block.html
