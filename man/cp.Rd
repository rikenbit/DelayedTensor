\docType{methods}
\name{cp-methods}
\alias{cp-methods}
\alias{cp}
\alias{cp,DelayedArray-method}
\title{Canonical Polyadic Decomposition}
\usage{
cp(darr, num_components=NULL, max_iter=25, tol=1e-05)

\S4method{cp}{DelayedArray}(darr, num_components, max_iter, tol)
}
\arguments{
\item{darr}{Tensor with K modes}

\item{num_components}{the number of rank-1 K-Tensors to use in approximation}

\item{max_iter}{maximum number of iterations if error stays above \code{tol}}

\item{tol}{relative Frobenius norm error tolerance}
}
\value{
a list containing the following \describe{
\item{\code{lambdas}}{a vector of normalizing constants, one for each component}
\item{\code{U}}{a list of matrices - one for each mode -
each matrix with \code{num_components} columns}
\item{\code{conv}}{whether or not \code{resid} < \code{tol}
by the last iteration}
\item{\code{norm_percent}}{the percent of Frobenius norm explained
by the approximation}
\item{\code{est}}{estimate of \code{darr} after compression}
\item{\code{fnorm_resid}}{the Frobenius norm of the error
\code{fnorm(est-darr)}}
\item{\code{all_resids}}{vector containing the Frobenius
norm of error for all the iterations}
}
}
\description{
Canonical Polyadic (CP) decomposition of a tensor, aka CANDECOMP/PARAFRAC.
Approximate a K-Tensor using a sum of \code{num_components} rank-1 K-Tensors.
A rank-1 K-Tensor can be written as an outer product of K vectors.
There are a total of \code{num_compoents *darr@num_modes} vectors in the output,
stored in \code{darr@num_modes} matrices,
each with \code{num_components} columns.
This is an iterative algorithm, with two possible stopping conditions:
either relative error in Frobenius norm has gotten below \code{tol},
or the \code{max_iter} number of iterations has been reached.
For more details on CP decomposition, consult Kolda and Bader (2009).
}
\details{
This function is an extension of the \code{\link[rTensor]{cp}}
by DelayedArray.

Uses the Alternating Least Squares (ALS) estimation procedure.
A progress bar is included to help monitor operations on large tensors.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(3,4,5))
cp(darr, num_components=2)
}
\references{
T. Kolda, B. Bader,
"Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{tucker}}
}
