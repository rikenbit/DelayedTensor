\docType{methods}
\name{rs_unfold-methods}
\alias{rs_unfold-methods}
\alias{rs_unfold}
\alias{rs_unfold,DelayedArray-method}
\title{Tensor Row Space Unfolding of DelayedArray}
\usage{
rs_unfold(darr, m)

\S4method{rs_unfold}{DelayedArray}(darr, m)
}
\arguments{
\item{darr}{DelayedArray object}

\item{m}{mode to be unfolded on}
}
\value{
DelayedArray (2D)
}
\description{
Please see \code{\link{k_unfold}} and \code{\link{unfold}}.
}
\details{
This function is an extension of the \code{\link[rTensor]{rs_unfold}}
by DelayedArray.

This is a wrapper function to \code{\link{unfold}}.

See also \code{rs_unfold(darr, m=NULL)}
}
\seealso{
\code{\link{unfold}}, \code{\link{rs_fold}}
}
