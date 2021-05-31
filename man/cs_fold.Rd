\docType{methods}
\name{cs_fold-methods}
\alias{cs_fold-methods}
\alias{cs_fold}
\alias{cs_fold,DelayedArray-method}
\title{Column Space Folding of 2D DelayedArray}
\usage{
cs_fold(mat, m = NULL, modes = NULL)

\S4method{cs_fold}{DelayedArray}(mat, m, modes)
}
\arguments{
\item{mat}{DelayedArray object (only 2D)}

\item{m}{the mode corresponding to cs_unfold}

\item{modes}{the original modes of the DelayedArray}
}
\value{
DelayedArray (higher than 2D)
}
\description{
The inverse operation to \code{\link{cs_unfold}}.
}
\details{
This function is an extension of the \code{\link[rTensor]{cs_fold}}
by DelayedArray.

This is a wrapper function to \code{\link{fold}}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
matT3 <- DelayedTensor::cs_unfold(darr, m=3)
identical(
    as.array(DelayedTensor::cs_fold(matT3, m=3, modes=c(2,3,4))),
    as.array(darr))
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{fold}}, \code{\link{cs_unfold}}
}
