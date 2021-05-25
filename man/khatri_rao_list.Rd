\name{khatri_rao_list}
\alias{khatri_rao_list}
\title{Khatri-Rao Product against list}
\usage{
khatri_rao_list(L, reverse = FALSE)
}
\arguments{
\item{L}{list of DelayedArray}

\item{reverse}{whether or not to reverse the order}
}
\value{
matrix that is the Khatri-Rao product
}
\description{
Returns the Khatri-Rao product from a list of matrices or vectors.
Commonly used for n-mode products and various Tensor decompositions.
}
\note{
The number of columns must match in every element of the input list.
}
\details{
This function is an extension of the \code{\link[rTensor]{khatri_rao_list}}
by DelayedArray.
}
\examples{
library("DelayedRandomArray")
darr1 <- RandomUnifArray(c(2,3))
dlizt <- list(
    'darr1' = RandomUnifArray(c(2,4)),
    'darr2' = RandomUnifArray(c(3,4)))
khatri_rao_list(dlizt)
}
\seealso{
\code{\link{hadamard}}, \code{\link{hadamard_list}},
\code{\link{kronecker}}, \code{\link{kronecker_list}},
\code{\link{khatri_rao}}
}
