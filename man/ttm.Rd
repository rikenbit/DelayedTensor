\docType{methods}
\name{ttm-methods}
\alias{ttm-methods}
\alias{ttm}
\alias{ttm,DelayedArray,DelayedArray-method}
\title{Tensor Times Matrix (m-Mode Product)}
\usage{
ttm(darr, mat, m = NULL)

\S4method{ttm}{DelayedArray,DelayedArray}(darr, mat, m)
}
\arguments{
\item{darr}{DelayedArray object}

\item{mat}{input 2D DelayedArray with same number columns as
the \code{m}th mode of \code{darr}}

\item{m}{the mode to contract on}
}
\value{
a DelayedArray object with K modes
}
\description{
Contracted (m-Mode) product between a DelayedArray (Tensor)
of arbitrary number of modes and a matrix.
The result is folded back into Tensor.
}
\details{
This function is an extension of the \code{\link[rTensor]{ttm}}
by DelayedArray.

By definition, \code{rs_unfold(ttm(darr, mat), m) = mat\%*\%rs_unfold(darr, m)},
so the number of columns in \code{mat} must match the
\code{m}th mode of \code{darr}. For the math on the m-Mode Product,
see Kolda and Bader (2009).
}
\note{
The \code{m}th mode of \code{darr} must match the number of columns in
\code{mat}. By default, the returned Tensor does not drop any modes equal to 1.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
mat <- RandomUnifArray(c(10,4))
ttm(darr, mat, m=3)
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{rs_unfold}}, \code{\link{ttl}}
}
