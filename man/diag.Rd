\docType{methods}
\name{diag-methods}
\alias{diag-methods}
\alias{diag}
\alias{diag<-}
\alias{diag,DelayedArray-method}
\alias{diag<-,DelayedArray-method}
\title{DelayedArray Diagonals}
\usage{
diag(darr)
diag(darr) <- value

\S4method{diag}{DelayedArray}(darr)
\S4method{diag}{DelayedArray}(darr) <- value
}
\arguments{
\item{darr}{DelayedArray object}

\item{value}{either a single value or a vector of length equal to that
    of the current diagonal. Should be of a mode which can be coerced
    to that of \code{darr}.}
}
\value{
1D DelayedArray (vector) with length \code{min(dim(darr))}
}
\description{
Extract or replace the diagonal of a DelayedArray,
or substitute the elements to the diagonal DelayedArray.
}
\details{
See also \code{\link{DelayedDiagonalArray}} or \code{\link[base]{diag}}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
DelayedTensor::diag(darr)
DelayedTensor::diag(darr)[1] <- 11111
DelayedTensor::diag(darr)[2] <- 22222
DelayedTensor::diag(darr)
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{DelayedDiagonalArray}}
}
