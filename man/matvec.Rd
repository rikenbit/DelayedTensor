\docType{methods}
\name{matvec-methods}
\alias{matvec-methods}
\alias{matvec}
\alias{matvec,DelayedArray-method}
\title{Tensor Matvec Unfolding of DelayedArray}
\usage{
matvec(darr)

\S4method{matvec}{DelayedArray}(darr)
}
\arguments{
\item{darr}{DelayedArray object}
}
\value{
matrix with \code{prod(dim(darr)[-m])} rows and \code{dim(darr)[m]} columns
}
\description{
For 3-tensors only.
Stacks the slices along the third mode.
}
\details{
This function is an extension of the \code{\link[rTensor]{matvec}}
by DelayedArray.

This is a wrapper function to \code{\link{unfold}}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
matvec(darr)
}
\references{
M. Kilmer, K. Braman, N. Hao, and R. Hoover,
"Third-order tensors as operators on matrices: a theoretical and
computational framework with applications in imaging".
SIAM Journal on Matrix Analysis and Applications 2013.
}
\seealso{
\code{\link{unfold}}, \code{\link{unmatvec}}
}
