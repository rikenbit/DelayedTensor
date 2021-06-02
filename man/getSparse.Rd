\name{getSparse}
\alias{getSparse}
\title{Getter of the intermediate/output DelayedArray object in DelayedTensor}
\usage{
getSparse()
}
\value{
TRUE or FALSE (Default: FALSE)
}
\description{
Whether the intermediate and output DelayedArray used in DelayedTensor
is used as sparse tensor or not.

NOTE: Sparse mode is experimental! Whether it contributes to higher speed and
lower memory is quite dependent on the sparsity of the DelayedArray,
and the current implementation does not recognize the block size,
which may cause Out-of-Memory errors.
}
\examples{
getSparse()
}
