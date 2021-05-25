\name{DelayedTensor-package}
\alias{DelayedTensor-package}
\alias{DelayedTensor}
\docType{package}
\title{
\packageTitle{DelayedTensor}
}
\description{
\packageDescription{DelayedTensor}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{DelayedTensor}
\packageIndices{DelayedTensor}
}
\author{
\packageAuthor{DelayedTensor}

Maintainer: \packageMaintainer{DelayedTensor}
}
\keyword{ package }
\seealso{
# Unfold operations
\code{\link{unfold}}, \code{\link{k_unfold}}, \code{\link{matvec}},
\code{\link{rs_unfold}}, \code{\link{cs_unfold}}, \code{\link{ttl}}
# Fold operations
\code{\link{fold}}, \code{\link{k_fold}}, \code{\link{unmatvec}},
\code{\link{rs_fold}}, \code{\link{cs_fold}}, \code{\link{ttm}}
# Vectorization
\code{\link{vec}}
# Norm operations
\code{\link{fnorm}}, \code{\link{innerProd}}
# Diagonal operations / Diagonal Tensor
\code{\link{diag}}, \code{\link{DelayedDiagonalArray}}
# Mode-wise operations
\code{\link{modeSum}}, \code{\link{modeMean}}
# Tensor product operations
\code{\link{hadamard}}, \code{\link{hadamard_list}},
\code{\link{kronecker}}, \code{\link{kronecker_list}},
\code{\link{khatri_rao}}, \code{\link{khatri_rao_list}}
# Utilities
\code{\link{list_rep}}, \code{\link{modebind_list}},
\code{\link{rbind_list}}, \code{\link{cbind_list}}
# Decomposition operations
\code{\link{hosvd}}, \code{\link{cp}}, \code{\link{tucker}},
\code{\link{mpca}}, \code{\link{pvd}}
# Einsum operation
\code{\link{einsum}}
}
\examples{
ls("package:DelayedTensor")
}