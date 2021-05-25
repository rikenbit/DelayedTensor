\docType{methods}
\name{pvd-methods}
\alias{pvd-methods}
\alias{pvd}
\alias{pvd,DelayedArray-method}
\title{Population Value Decomposition}
\usage{
pvd(darr, uranks=NULL, wranks=NULL, a=NULL, b=NULL)

\S4method{pvd}{DelayedArray}(darr, uranks, wranks, a, b)
}
\arguments{
\item{darr}{3D DelayedArray (Tensor) with the third mode
being the measurement mode}

\item{uranks}{ranks of the U matrices}

\item{wranks}{ranks of the W matrices}

\item{a}{rank of \code{P = U\%*\%t(U)}}

\item{b}{rank of \code{D = W\%*\%t(W)}}
}
\value{
a list containing the following:\describe{
\item{\code{P}}{population-level matrix \code{P = U\%*\%t(U)},
where U is constructed by stacking the truncated left eigenvectors
of slicewise PCA along the third mode}
\item{\code{V}}{a list of image-level core matrices}
\item{\code{D}}{population-leve matrix \code{D = W\%*\%t(W)},
where W is constructed by stacking the truncated right eigenvectors
of slicewise PCA along the third mode}
\item{\code{est}}{estimate of \code{darr} after compression}
\item{\code{norm_percent}}{
the percent of Frobenius norm explained by the approximation}
\item{\code{fnorm_resid}}{
the Frobenius norm of the error \code{fnorm(est-darr)}}
}
}
\description{
The default Population Value Decomposition (PVD) of a series of 2D images.
Constructs population-level matrices P, V, and D to account for variances
within as well as across the images. Structurally similar to Tucker
(\code{\link{tucker}}) and GLRAM (\code{\link{mpca}}),
but retains crucial differences. Requires \code{2*n3 + 2} parameters to
specified the final ranks of P, V, and D, where n3 is the third mode
(how many images are in the set). Consult Crainiceanu et al. (2013)
for the construction and rationale behind the PVD model.
}
\details{
This function is an extension of the \code{\link[rTensor]{pvd}}
by DelayedArray.

The PVD is not an iterative method, but instead relies on \code{n3 + 2}
separate PCA decompositions.
The third mode is for how many images are in the set.
}
\examples{
library("DelayedRandomArray")
darr <- RandomUnifArray(c(3,4,5))
pvd(darr, uranks=rep(2,5), wranks=rep(3,5), a=2, b=3)
}
\references{
C. Crainiceanu, B. Caffo, S. Luo, V. Zipunnikov, N. Punjabi,
"Population value decomposition:
a framework for the analysis of image populations".
Journal of the American Statistical Association, 2013.
}
