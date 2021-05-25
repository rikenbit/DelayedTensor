\docType{methods}
\name{k_unfold-methods}
\alias{k_unfold-methods}
\alias{k_unfold}
\alias{k_unfold,DelayedArray-method}
\title{Tensor k-mode Unfolding of DelayedArray}
\usage{
k_unfold(darr, m)

\S4method{k_unfold}{DelayedArray}(darr, m)
}
\arguments{
\item{darr}{DelayedArray object}

\item{m}{the index of the mode to unfold on}
}
\value{
matrix with \code{dim(darr)[m]} rows and \code{prod(dim(darr)[-m])} columns
}
\description{
Unfolding of a tensor by mapping the kth mode
(specified through parameter \code{m}),
and all other modes onto the column space.
This the most common type of unfolding operation for
Tucker decompositions and its variants.
Also known as k-mode matricization.
}
\details{
This function is an extension of the \code{\link[rTensor]{k_unfold}}
by DelayedArray.

This is a wrapper function to \code{\link{unfold}}.

See also \code{k_unfold(darr, m=NULL)}
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
rs_unfold(darr, m=2)
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{unfold}}, \code{\link{k_fold}}
}
