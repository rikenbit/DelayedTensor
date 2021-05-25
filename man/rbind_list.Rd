\name{rbind_list}
\alias{rbind_list}
\title{Mode-binding against list}
\usage{
rbind_list(L)
}
\arguments{
\item{L}{list of 2D DelayedArray}
}
\value{
2D DelayedArray object
}
\description{
Returns the binded DelayedArray in row space.
}
\details{
This is a wrapper function to \code{\link{modebind_list}},
when the DelayedArrays are 2D.
}
\note{
The dimensions of row in each DelayedArray must match.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
dlizt <- list(
    'darr1' = RandomUnifArray(c(2,3)),
    'darr2' = RandomUnifArray(c(2,3)))
rbind_list(dlizt)
}
\seealso{
\code{\link{modebind_list}}
}
