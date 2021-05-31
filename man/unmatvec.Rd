\docType{methods}
\name{unmatvec-methods}
\alias{unmatvec-methods}
\alias{unmatvec}
\alias{unmatvec,DelayedArray-method}
\title{Unmatvec Folding of 2D DelayedArray}
\usage{
unmatvec(mat, modes = NULL)

\S4method{unmatvec}{DelayedArray}(mat, modes)
}
\arguments{
\item{mat}{DelayedArray object (only 2D)}

\item{modes}{the modes of the output DelayedArray}
}
\value{
DelayedArray object with modes given by \code{modes}
}
\description{
The inverse operation to \code{\link{matvec-methods}},
turning a matrix into a Tensor. For a full account of matrix
folding/unfolding operations, consult Kolda and Bader (2009).
}
\details{
This function is an extension of the \code{\link[rTensor]{unmatvec}}
by DelayedArray.

This is a wrapper function to \code{\link{fold}}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
matT1 <- matvec(darr)
identical(
    as.array(unmatvec(matT1, modes=c(2,3,4))),
    as.array(darr))
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{fold}}, \code{\link{matvec}}
}
