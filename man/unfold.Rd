\docType{methods}
\name{unfold-methods}
\alias{unfold-methods}
\alias{unfold}
\alias{unfold,DelayedArray-method}
\title{Tensor Unfolding of 2D DelayedArray}
\usage{
unfold(darr, row_idx, col_idx)

\S4method{unfold}{DelayedArray}(darr, row_idx, col_idx)
}
\arguments{
\item{darr}{DelayedArray object}

\item{row_idx}{the indices of the modes to map onto the row space}

\item{col_idx}{the indices of the modes to map onto the column space}
}
\value{
2D DelayedArray with \code{prod(row_idx)} rows and \code{prod(col_idx)} columns
}
\description{
Unfolds the tensor into a matrix, with the modes in \code{rs} onto the rows
and modes in \code{cs} onto the columns. Note that \code{c(rs,cs)}
must have the same elements (order doesn't matter) as \code{dim(darr)}.
Within the rows and columns, the order of the unfolding is determined
by the order of the modes.
This convention is consistent with Kolda and Bader (2009).
}
\details{
This function is an extension of the \code{\link[rTensor]{unfold}}
by DelayedArray.

For Row Space Unfolding or m-mode Unfolding,
see \code{\link{rs_unfold}}.
For Column Space Unfolding or matvec,
see \code{\link{cs_unfold}}.

\code{\link{vec}} returns the vectorization of the tensor.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
unfold(darr, row_idx=2, col_idx=c(3,1))
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{k_unfold}}, \code{\link{matvec}},
\code{\link{rs_unfold}}, \code{\link{cs_unfold}}
}
