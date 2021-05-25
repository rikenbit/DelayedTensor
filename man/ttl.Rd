\name{ttl}
\alias{ttl}
\title{DelayedArray Times List}
\usage{
ttl(darr, list_mat, ms=NULL)
}
\arguments{
\item{darr}{DelayedArray object with K modes}

\item{list_mat}{a list of 2D DelayedArray objects}

\item{ms}{a vector of modes to contract on
(order should match the order of \code{list_mat})}
}
\value{
DelayedArray object with K modes (Tensor)
}
\description{
Contracted (m-Mode) product between a Tensor of arbitrary number of modes
and a list of matrices. The result is folded back into Tensor.
}
\details{
This function is an extension of the \code{\link[rTensor]{ttl}}
by DelayedArray.

This is a wrapper function to \code{\link{unfold}}.

Performs \code{ttm} repeated for a single Tensor and
a list of matrices on multiple modes. For instance,
suppose we want to do multiply a Tensor object \code{darr} with
three matrices \code{mat1}, \code{mat2}, \code{mat3} on modes 1, 2, and 3.
We could do \code{ttm(ttm(ttm(darr,mat1,1),mat2,2),3)},
or we could do \code{ttl(darr,list(mat1,mat2,mat3),c(1,2,3))}.
The order of the matrices in the list should obviously match
the order of the modes.
This is a common operation for various Tensor decompositions
such as CP and Tucker.
For the math on the m-Mode Product, see Kolda and Bader (2009).
}
\note{
The returned Tensor does not drop any modes equal to 1.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(3,4,5))
dlizt <- list(
    'darr1' = RandomUnifArray(c(10,3)),
    'darr2' = RandomUnifArray(c(10,4)))
ttl(darr, dlizt, ms=c(1,2))
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{ttm}}
}
