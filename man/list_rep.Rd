\name{list_rep}
\alias{list_rep}
\title{Replicate of arbitrary object}
\usage{
list_rep(x, n=NULL)
}
\arguments{
\item{x}{Any object}

\item{n}{Number of replicate}
}
\value{
List
}
\description{
Returns the replicates of base obejct \code{x}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
list_rep(darr, 3)
}
