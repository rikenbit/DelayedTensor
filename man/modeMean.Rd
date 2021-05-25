\docType{methods}
\name{modeMean-methods}
\alias{modeMean-methods}
\alias{modeMean}
\alias{modeMean,DelayedArray-method}
\title{Tensor Mean Across Single Mode of DelayedArray}
\usage{
modeMean(darr, m = NULL, drop = FALSE)

\S4method{modeMean}{DelayedArray}(darr, m, drop)
}
\arguments{
\item{darr}{DelayedArray object}

\item{m}{the index of the mode to average across}

\item{drop}{whether or not mode m should be dropped}
}
\value{
K-1 or K Tensor, where \code{K = length(dim(darr))}
}
\description{
Given a mode for a K-tensor, this returns the K-1 tensor resulting from
taking the mean across that particular mode.
}
\details{
This function is an extension of the \code{\link[rTensor]{modeMean}}
by DelayedArray.

\code{modeMean(darr, m=NULL, drop=FALSE)}
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(1,2,3))
modeMean(darr, 1, drop=FALSE)
modeMean(darr, 1, drop=TRUE)
modeMean(darr, 2)
modeMean(darr, 3)
}
\seealso{
\code{\link{modeSum}}
}
