\name{DelayedDiagonalArray}
\alias{DelayedDiagonalArray}
\title{Diagonal DelayedArray}
\usage{
DelayedDiagonalArray(shape, value)
}
\arguments{
\item{shape}{Shape of DelayedArray (mode of Tensor)}

\item{value}{either a single value or a vector. This argument is optional.
If nothing is specified, 1s are filled with each diagonal element.}
}
\value{
DelayedArray object
}
\description{
Constructor of the diagonal of a DelayedArray.
}
\details{
See also \code{\link{diag}} or \code{\link[base]{diag}}.
}
\examples{
darr <- DelayedDiagonalArray(2:4, 5)
DelayedTensor::diag(darr)
}
\references{
T. Kolda, B. Bader, "Tensor decomposition and applications".
SIAM Applied Mathematics and Applications 2009.
}
\seealso{
\code{\link{diag}}, \code{\link[base]{diag}}
}
