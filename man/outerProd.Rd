\docType{methods}
\name{outerProd-methods}
\alias{outerProd-methods}
\alias{outerProd}
\alias{outerProd,DelayedArray,DelayedArray-method}
\title{Tensors Outer Product of DelayedArray}
\usage{
outerProd(darr1, darr2)

\S4method{outerProd}{DelayedArray,DelayedArray}(darr1, darr2)
}
\arguments{
\item{darr1}{first DelayedArray object}

\item{darr2}{second DelayedArray object}
}
\value{
outer product between \code{darr1} and \code{darr2}
}
\description{
Returns the outer product between two Tensors
}
\details{
NOTE: Sparse mode of outerProd is not available for now.
}
\examples{
library("DelayedRandomArray")
darr1 <- RandomUnifArray(c(2,3))
darr2 <- RandomUnifArray(c(4,5))
outerProd(darr1, darr2)
}
