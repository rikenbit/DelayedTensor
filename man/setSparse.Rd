\name{setSparse}
\alias{setSparse}
\title{Setter to set the intermediate DelayedArray object in DelayedTensor}
\usage{
setSparse(as.sparse=FALSE)
}
\arguments{
\item{as.sparse}{TRUE or FALSE (Default: FALSE)}
}
\value{Nothing}
\description{
Set whether the intermediate and output DelayedArray used in DelayedTensor
is used as sparse tensor or not.

NOTE: Sparse mode is experimental! Whether it contributes to higher speed and
lower memory is quite dependent on the sparsity of the DelayedArray,
and the current implementation does not recognize the block size,
which may cause Out-of-Memory errors.
}
\examples{
setSparse(TRUE)
setSparse(FALSE)
}
