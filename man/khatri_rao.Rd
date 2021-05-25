\docType{methods}
\name{khatri_rao-methods}
\alias{khatri_rao-methods}
\alias{khatri_rao}
\alias{khatri_rao,DelayedArray,DelayedArray-method}
\title{Khatri-Rao Product of DelayedArray}
\usage{
khatri_rao(darr1, darr2)

\S4method{khatri_rao}{DelayedArray,DelayedArray}(darr1, darr2)
}
\arguments{
\item{darr1}{first DelayedArray object}

\item{darr2}{second DelayedArray object}
}
\value{
matrix that is the Khatri-Rao product
}
\description{
Returns the Khatri-Rao (column-wise Kronecker) product of two matrices.
If the inputs are vectors then this is the same as the Kronecker product.
}
\note{
The number of columns must match in the two inputs.
}
\details{
This function is an extension of the \code{\link[rTensor]{khatri_rao}}
by DelayedArray.
}
\examples{
library("DelayedRandomArray")
darr1 <- RandomUnifArray(c(2,4))
darr2 <- RandomUnifArray(c(3,4))
khatri_rao(darr1, darr2)
}
\seealso{
\code{\link{hadamard}}, \code{\link{hadamard_list}},
\code{\link{kronecker}}, \code{\link{kronecker_list}},
\code{\link{khatri_rao_list}}
}
