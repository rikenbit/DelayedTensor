\name{hadamard_list}
\alias{hadamard_list}
\title{Hadamard Product against list}
\usage{
hadamard_list(L)
}
\arguments{
\item{L}{list of DelayedArray}
}
\value{
matrix that is the Hadamard product
}
\description{
Returns the hadamard (element-wise) product from a list of matrices or vectors.
Commonly used for n-mode products and various Tensor decompositions.
}
\details{
This function is an extension of the \code{\link[rTensor]{hadamard_list}}
by DelayedArray.
}
\note{
The modes/dimensions of each element in the list must match.
}
\examples{
library("DelayedRandomArray")
dlizt <- list(
    'darr1' = RandomUnifArray(c(2,3,4)),
    'darr2' = RandomUnifArray(c(2,3,4)))
hadamard_list(dlizt)
}
\seealso{
\code{\link{khatri_rao}}, \code{\link{khatri_rao_list}},
\code{\link{kronecker}}, \code{\link{kronecker_list}},
\code{\link{hadamard}}
}
