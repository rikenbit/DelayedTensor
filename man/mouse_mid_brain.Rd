\name{mouse_mid_brain}
\alias{mouse_mid_brain}
\docType{data}
\title{Matrix object of mouse mid brain data}
\description{
A matrix with 500 rows (genes) * 1907 columns (cells).
}
\usage{data(mouse_mid_brain)}
\details{
The data matrix is downloaded from GEO Series GSE76381
(https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE76381&format=file).
% For saving the package size, the number of genes are strictlly reduced by the
% thresholding based on the variances of genes.
For the details, see inst/script/make-data.R.
}
\references{
Y-h. Taguchi and T. Turki (2019)
Tensor Decomposition-Based Unsupervised Feature Extraction Applied to
Single-Cell Gene Expression Analysis.
\emph{Frontiers in Genetics}, \bold{10(864)}: 10:3389/fgene.2019.00864
}
\examples{
data(mouse_mid_brain)
}
\seealso{
\code{\link{mouse_mid_brain}}
}
\keyword{datasets}
