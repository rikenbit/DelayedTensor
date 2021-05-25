\name{cbind_list}
\alias{cbind_list}
\title{Mode-binding against list}
\usage{
cbind_list(L)
}
\arguments{
\item{L}{list of 2D DelayedArray}
}
\value{
2D DelayedArray object
}
\description{
Returns the binded DelayedArray in column space.
}
\details{
This is a wrapper function to \code{\link{modebind_list}},
when the DelayedArrays are 2D.
}
\note{
The dimensions of column in each DelayedArray must match.
}
\examples{
library("DelayedRandomArray")
dlizt <- list(
    'darr1' = RandomUnifArray(c(2,3)),
    'darr2' = RandomUnifArray(c(2,3)))
cbind_list(dlizt)
}
\seealso{
\code{\link{modebind_list}}
}
