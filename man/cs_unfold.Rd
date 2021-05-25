\docType{methods}
\name{cs_unfold-methods}
\alias{cs_unfold-methods}
\alias{cs_unfold}
\alias{cs_unfold,DelayedArray-method}
\title{Tensor Column Space Unfolding of DelayedArray}
\usage{
cs_unfold(darr, m)

\S4method{cs_unfold}{DelayedArray}(darr, m)
}
\arguments{
\item{darr}{DelayedArray object}

\item{m}{mode to be unfolded on}
}
\value{
DelayedArray (2D)
}
\description{
Please see \code{\link{matvec}} and \code{\link{unfold}}.
}
\details{
This function is an extension of the \code{\link[rTensor]{cs_unfold}}
by DelayedArray.

This is a wrapper function to \code{\link{unfold}}.
}
\seealso{
\code{\link{unfold}}, \code{\link{cs_fold}}
}
