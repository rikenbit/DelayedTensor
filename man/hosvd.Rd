\docType{methods}
\name{hosvd-methods}
\alias{hosvd-methods}
\alias{hosvd}
\alias{hosvd,DelayedArray-method}
\title{(Truncated-)Higher-order SVD}
\usage{
hosvd(darr, ranks=NULL)

\S4method{hosvd}{DelayedArray}(darr, ranks)
}
\arguments{
\item{darr}{Tensor with K modes}

\item{ranks}{a vector of desired modes in the output core tensor,
default is \code{darr@modes}}
}
\value{
a list containing the following:\describe{
\item{\code{Z}}{core tensor with modes speficied by \code{ranks}}
\item{\code{U}}{a list of orthogonal matrices, one for each mode}
\item{\code{est}}{estimate of \code{darr} after compression}
\item{\code{fnorm_resid}}{the Frobenius norm of the error
\code{fnorm(est-darr)} - if there was no truncation,
then this is on the order of mach_eps * fnorm. }
}
}
\description{
Higher-order SVD of a K-Tensor.
Write the K-Tensor as a (m-mode) product of a core Tensor
(possibly smaller modes) and K orthogonal factor matrices.
Truncations can be specified via \code{ranks}
(making them smaller than the original modes of the K-Tensor will
result in a truncation).
For the mathematical details on HOSVD, consult Lathauwer et. al. (2000).
}
\details{
This function is an extension of the \code{\link[rTensor]{hosvd}}
by DelayedArray.

A progress bar is included to help monitor operations on large tensors.
}
\note{
The length of \code{ranks} must match \code{darr@num_modes}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(3,4,5))
hosvd(darr, ranks=c(2,1,3))
}
\references{
L. Lathauwer, B.Moor, J. Vanderwalle
"A multilinear singular value decomposition".
Journal of Matrix Analysis and Applications 2000.
}
\seealso{
\code{\link{tucker}}
}
