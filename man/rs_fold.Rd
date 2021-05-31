\docType{methods}
\name{rs_fold-methods}
\alias{rs_fold-methods}
\alias{rs_fold}
\alias{rs_fold,DelayedArray-method}
\title{Row Space Folding of 2D DelayedArray}
\usage{
rs_fold(mat, m = NULL, modes = NULL)

\S4method{rs_fold}{DelayedArray}(mat, m, modes)
}
\arguments{
\item{mat}{DelayedArray object (only 2D)}

\item{m}{the mode corresponding to rs_unfold}

\item{modes}{the original modes of the DelayedArray}
}
\value{
DelayedArray (higher than 2D)
}
\description{
The inverse operation to \code{\link{rs_unfold}}.
}
\details{
This function is an extension of the \code{\link[rTensor]{rs_fold}}
by DelayedArray.

This is a wrapper function to \code{\link{fold}}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
matT2 <- rs_unfold(darr, m=2)
identical(
    as.array(rs_fold(matT2, m=2, modes=c(2,3,4))),
    as.array(darr))
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{fold}}, \code{\link{rs_unfold}}
}
