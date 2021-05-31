\docType{methods}
\name{k_fold-methods}
\alias{k_fold-methods}
\alias{k_fold}
\alias{k_fold,DelayedArray-method}
\title{k-mode Folding  of 2D DelayedArray}
\usage{
k_fold(mat, m = NULL, modes = NULL)

\S4method{k_fold}{DelayedArray}(mat, m, modes)
}
\arguments{
\item{mat}{DelayedArray object (only 2D)}

\item{m}{the index of the mode that is mapped onto the row indices}

\item{modes}{the modes of the output DelayedArray}
}
\value{
DelayedArray object with modes given by \code{modes}
}
\description{
k-mode folding of a matrix into a Tensor. This is the inverse funtion
to \code{k_unfold} in the m mode. In particular,
\code{k_fold(k_unfold(darr, m), m, dim(darr))}
will result in the original Tensor.
}
\details{
This function is an extension of the \code{\link[rTensor]{k_fold}}
by DelayedArray.

This is a wrapper function to \code{\link{fold}}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
matT2 <- k_unfold(darr, m=2)
identical(
    as.array(k_fold(matT2, m=2, modes=c(2,3,4))),
    as.array(darr))
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{fold}}, \code{\link{k_unfold}}
}
