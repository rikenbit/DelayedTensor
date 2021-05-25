\docType{methods}
\name{fnorm-methods}
\alias{fnorm-methods}
\alias{fnorm}
\alias{fnorm,DelayedArray-method}
\title{Tensor Frobenius Norm of DelayedArray}
\usage{
fnorm(darr)

\S4method{fnorm}{DelayedArray}(darr)
}
\arguments{
\item{darr}{DelayedArray object}
}
\value{
numeric Frobenius norm of \code{darr}
}
\description{
Returns the Frobenius norm of the Tensor instance.
}
\details{
This function is an extension of the \code{\link[rTensor]{fnorm}}
by DelayedArray.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(2,3,4))
fnorm(darr)
}
