\docType{methods}
\name{mpca-methods}
\alias{mpca-methods}
\alias{mpca}
\alias{mpca,DelayedArray-method}
\title{Multilinear Principal Components Analysis}
\usage{
mpca(darr, ranks=NULL, max_iter=25, tol=1e-05)

\S4method{mpca}{DelayedArray}(darr, ranks, max_iter, tol)
}
\arguments{
\item{darr}{Tensor with K modes}

\item{ranks}{a vector of the compressed modes of the output core Tensor,
this has length K-1}

\item{max_iter}{maximum number of iterations if error stays above \code{tol}}

\item{tol}{relative Frobenius norm error tolerance}
}
\value{
a list containing the following:\describe{
\item{\code{Z_ext}}{the extended core tensor,
with the first K-1 modes given by \code{ranks}}
\item{\code{U}}{a list of K-1 orthgonal factor matrices -
one for each compressed mode, with the number of columns
of the matrices given by \code{ranks}}
\item{\code{conv}}{whether or not \code{resid} < \code{tol}
by the last iteration}
\item{\code{est}}{estimate of \code{darr} after compression}
\item{\code{norm_percent}}{the percent of Frobenius norm
explained by the approximation}
\item{\code{fnorm_resid}}{the Frobenius norm of the error
\code{fnorm(est-darr)}}
\item{\code{all_resids}}{vector containing the Frobenius norm of error
for all the iterations}
}
}
\description{
This is basically the Tucker decomposition of a K-Tensor,
\code{\link{tucker}}, with one of the modes uncompressed.
If K = 3, then this is also known as the Generalized Low Rank Approximation
of Matrices (GLRAM). This implementation assumes that the last mode is the
measurement mode and hence uncompressed. This is an iterative algorithm,
with two possible stopping conditions:
either relative error in Frobenius norm has gotten below \code{tol},
or the \code{max_iter} number of iterations has been reached.
For more details on the MPCA of tensors, consult Lu et al. (2008).
}
\details{
This function is an extension of the \code{\link[rTensor]{mpca}}
by DelayedArray.

Uses the Alternating Least Squares (ALS) estimation procedure.
A progress bar is included to help monitor operations on large tensors.
}
\note{
The length of \code{ranks} must match \code{darr@num_modes-1}.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(3,4,5))
mpca(darr, ranks=c(1,2))
}
\references{
H. Lu, K. Plataniotis, A. Venetsanopoulos,
"Mpca: Multilinear principal component analysis of tensor objects".
IEEE Trans. Neural networks, 2008.
}
\seealso{
\code{\link{tucker}}, \code{\link{hosvd}}
}
