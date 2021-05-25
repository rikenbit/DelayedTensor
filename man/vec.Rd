\docType{methods}
\name{vec-methods}
\alias{vec-methods}
\alias{vec}
\alias{vec,DelayedArray-method}
\title{Tensor Vectorization of DelayedArray}
\usage{
vec(darr)

\S4method{vec}{DelayedArray}(darr)
}
\arguments{
\item{darr}{DelayedArray object}
}
\value{
1D DelayedArray (vector) with length \code{prod(dim(darr))}
}
\description{
Change the dimension of DelayedArray from multi-dimension (e.g. array) to
single-dimension (e.g. vector).
}
\details{
This function is an extension of the \code{\link[rTensor]{vec}}
by DelayedArray.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
vec(darr)
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
