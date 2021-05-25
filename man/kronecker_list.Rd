\name{kronecker_list}
\alias{kronecker_list}
\title{Kronecker Product against list}
\usage{
kronecker_list(L)
}
\arguments{
\item{L}{list of DelayedArray}
}
\value{
matrix that is the Kronecker product
}
\description{
Returns the Kronecker product from a list of matrices or vectors.
Commonly used for n-mode products and various Tensor decompositions.
}
\details{
This function is an extension of the \code{\link[rTensor]{kronecker_list}}
by DelayedArray.
}
\examples{
library("DelayedRandomArray")
dlizt <- list(
    'darr1' = RandomUnifArray(c(2,3,4)),
    'darr2' = RandomUnifArray(c(2,3,4)))
kronecker_list(dlizt)
}
\seealso{
\code{\link{khatri_rao}}, \code{\link{khatri_rao_list}},
\code{\link{hadamard}}, \code{\link{hadamard_list}},
\code{\link{kronecker}}
}
