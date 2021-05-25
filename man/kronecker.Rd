\docType{methods}
\name{kronecker-methods}
\alias{kronecker-methods}
\alias{kronecker}
\alias{kronecker,DelayedArray,DelayedArray-method}
\title{Kronecker Product of DelayedArray}
\usage{
kronecker(darr1, darr2)

\S4method{kronecker}{DelayedArray,DelayedArray}(darr1, darr2)
}
\arguments{
\item{darr1}{first DelayedArray object}

\item{darr2}{second DelayedArray object}
}
\value{
matrix that is the Kronecker product
}
\description{
Returns the Kronecker product of two Tensors.
Commonly used for n-mode products and various Tensor decompositions.
}
\examples{
library("DelayedRandomArray")
darr1 <- RandomUnifArray(c(2,3))
darr2 <- RandomUnifArray(c(4,5))
kronecker(darr1, darr2)
}
\seealso{
\code{\link{khatri_rao}}, \code{\link{khatri_rao_list}},
\code{\link{hadamard}}, \code{\link{hadamard_list}},
\code{\link{kronecker_list}}
}
