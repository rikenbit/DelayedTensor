\name{setSparse}
\alias{setSparse}
\title{Setter to set the intermediate DelayedArray object in DelayedTensor}
\usage{
setSparse(as.sparse=TRUE)
}
\arguments{
\item{as.sparse}{TRUE or FALSE (Default: TRUE)}
}
\value{Nothing}
\description{
Set whether the intermediate and output DelayedArray used in DelayedTensor
is used as sparse tensor or not.
}
\examples{
setSparse(TRUE)
setSparse(FALSE)
}
