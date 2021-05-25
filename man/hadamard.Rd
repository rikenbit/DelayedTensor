\docType{methods}
\name{hadamard-methods}
\alias{hadamard-methods}
\alias{hadamard}
\alias{hadamard,DelayedArray,DelayedArray-method}
\title{Hadamard Product of DelayedArray}
\usage{
hadamard(darr1, darr2)

\S4method{hadamard}{DelayedArray,DelayedArray}(darr1, darr2)
}
\arguments{
\item{darr1}{first DelayedArray object}

\item{darr2}{second DelayedArray object}
}
\value{
matrix that is the Hadamard product
}
\description{
Returns the Hadamard product of two Tensors.
Commonly used for n-mode products and various Tensor decompositions.
}
\note{
The modes/dimensions of each element of two Tensors must match.
}
\examples{
library("DelayedRandomArray")
darr1 <- RandomUnifArray(c(2,4))
darr2 <- RandomUnifArray(c(2,4))
hadamard(darr1, darr1)
}
\seealso{
\code{\link{khatri_rao}}, \code{\link{khatri_rao_list}},
\code{\link{kronecker}}, \code{\link{kronecker_list}},
\code{\link{hadamard_list}}
}
