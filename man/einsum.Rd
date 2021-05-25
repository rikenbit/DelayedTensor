\name{einsum}
\alias{einsum}
\title{Einstein Summation of DelayedArray}
\usage{
einsum(subscripts, ...)
}
\arguments{
\item{subscripts}{
a string in Einstein notation where arrays
are separated by ',' and the result is separated by '->'. For
example \code{"ij,jk->ik"} corresponds to a standard matrix multiplication.
Whitespace inside the \code{subscripts} is ignored. Unlike the
equivalent functions in Python, \code{einsum} only supports the explicit
mode. This means that the \code{subscripts} must contain '->'.}

\item{...}{the DelayedArrays that are combined.}
}
\value{
The \code{einsum} function returns an array with one dimension for each index
in the result of the \code{subscripts}.
For example \code{"ij,jk->ik"} produces a 2-dimensional array,
 \code{"abc,cd,de->abe"} produces a 3-dimensional array.
}
\description{
Einstein summation is a convenient and concise notation for operations
on n-dimensional arrays.
}
\details{
This function is an extension of the \code{\link[einsum]{einsum}}
by DelayedArray.
}
\examples{
library("DelayedArray")
library("DelayedRandomArray")
darr1 <- RandomUnifArray(c(4,8))
darr2 <- RandomUnifArray(c(8,3))

# # Matrix Multiply
# darr1 \%*\% darr2
# einsum("ij,jk -> ik", darr1, darr2)

# # Diag
# mat_sq <- RandomUnifArray(c(4,4))
# diag(mat_sq)
# einsum("ii->i", mat_sq)
# einsum("ii->ii", mat_sq)

# # Trace
# sum(diag(mat_sq))
# einsum("ii->", mat_sq)

# # Scalar product
# darr3 <- RandomUnifArray(c(4,8))
# darr3 * darr1
# einsum("ij,ij->ij", darr3, darr1)

# # Transpose
# t(darr1)
# einsum("ij->ji", darr1)

# # Batched L2 norm
# # arr1 <- array(c(darr1, darr3), dim = c(dim(darr1), 2))
# arr1 <- as.array(darr1)
# arr3 <- as.array(darr3)
# darr4 <- DelayedArray(array(c(arr1, arr3), dim = c(dim(arr1), 2)))

# c(sum(darr1^2), sum(darr3^2))
# einsum("ijb,ijb->b", darr4, darr4)
}