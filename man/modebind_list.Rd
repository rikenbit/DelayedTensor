\name{modebind_list}
\alias{modebind_list}
\title{Mode-binding against list}
\usage{
modebind_list(L, m=NULL)
}
\arguments{
\item{L}{list of DelayedArray}

\item{m}{list of DelayedArray}
}
\value{
DelayedArray object
}
\description{
Returns the binded DelayedArray in mode-\code{m}.
}
\note{
The dimensions of mode \code{m} must match.
}
\examples{
library("DelayedRandomArray")
dlizt <- list(
    'darr1' = RandomUnifArray(c(2,3,4)),
    'darr2' = RandomUnifArray(c(2,3,4)))
modebind_list(dlizt, m=1)
modebind_list(dlizt, m=2)
modebind_list(dlizt, m=3)
}
\seealso{
\code{\link{rbind_list}}, \code{\link{cbind_list}}
}
