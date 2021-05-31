\docType{methods}
\name{fold-methods}
\alias{fold-methods}
\alias{fold}
\alias{fold,DelayedArray-method}
\title{Tensor folding of 2D DelayedArray}
\usage{
fold(mat, row_idx = NULL, col_idx = NULL, modes = NULL)

\S4method{fold}{DelayedArray}(mat, row_idx, col_idx, modes)
}
\arguments{
\item{mat}{DelayedArray object (only 2D)}

\item{row_idx}{the indices of the modes that are mapped onto the row space}

\item{col_idx}{the indices of the modes that are mapped onto the column space}

\item{modes}{the modes of the output DelayedArray}
}
\value{
DelayedArray object with modes given by \code{modes}
}
\description{
General folding of a 2D DelayedArray into a higher-order DelayedArray(Tensor).
This is designed to be the inverse function to
\code{\link{unfold}}, with the same ordering of the indices.
This amounts to following: if we were to unfold a Tensor using a set of
\code{row_idx} and \code{col_idx}, then we can fold the resulting matrix
back into the original Tensor using the same \code{row_idx} and \code{col_idx}.
}
\details{
This function is an extension of the \code{\link[rTensor]{fold}}
by DelayedArray.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
matT3 <- DelayedTensor::unfold(darr, row_idx=2, col_idx=c(3,1))
identical(
    as.array(DelayedTensor::fold(matT3, row_idx=2,col_idx=c(3,1),
        modes=c(2,3,4))),
    as.array(darr))
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{unfold}}, \code{\link{k_fold}},
\code{\link{unmatvec}},
\code{\link{rs_fold}}, \code{\link{cs_fold}}
}
