\docType{methods}
\name{innerProd-methods}
\alias{innerProd-methods}
\alias{innerProd}
\alias{innerProd,DelayedArray,DelayedArray-method}
\title{Tensors Inner Product of DelayedArray}
\usage{
innerProd(darr1, darr2)

\S4method{innerProd}{DelayedArray,DelayedArray}(darr1, darr2)
}
\arguments{
\item{darr1}{first DelayedArray object}

\item{darr2}{second DelayedArray object}
}
\value{
inner product between \code{darr1} and \code{darr2}
}
\description{
Returns the inner product between two Tensors
}
\details{
This function is an extension of the \code{\link[rTensor]{innerProd}}
by DelayedArray.
}
\examples{
library("DelayedRandomArray")
darr1 <- RandomUnifArray(c(2,3,4))
darr2 <- RandomUnifArray(c(2,3,4))
innerProd(darr1, darr2)
}
