\docType{methods}
\name{modeSum-methods}
\alias{modeSum-methods}
\alias{modeSum}
\alias{modeSum,DelayedArray-method}
\title{Tensor Sum Across Single Mode of DelayedArray}
\usage{
modeSum(darr, m = NULL, drop = FALSE)

\S4method{modeSum}{DelayedArray}(darr, m, drop)
}
\arguments{
\item{darr}{DelayedArray object}

\item{m}{the index of the mode to sum across}

\item{drop}{whether or not mode m should be dropped}
}
\value{
K-1 or K tensor, where \code{K = length(dim(darr))}
}
\description{
Given a mode for a K-tensor, this returns the K-1 tensor resulting from
summing across that particular mode.
}
\details{
This function is an extension of the \code{\link[rTensor]{modeSum}}
by DelayedArray.

NOTE: Sparse mode of modeSum is not available for now.

\code{modeSum(darr, m=NULL, drop=FALSE)}
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(1,2,3))
modeSum(darr, 1, drop=FALSE)
modeSum(darr, 1, drop=TRUE)
modeSum(darr, 2)
modeSum(darr, 3)
}
\seealso{
\code{\link{modeMean}}
}
