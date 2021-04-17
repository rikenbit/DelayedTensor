# DelayedTensor
R package for sparse and out-of-core arithmetic and decomposition of Tensor.
DelayedTensor operates Tensor arithmetic directly on DelayedArray.
DelayedTensor provides some generic function related to Tensor arithmetic/decompotision and dispatches it on the DelayedArray class.
DelayedTensor also suppors Tensor contraction by einsum function, which is inspired by numpy einsum.

Installation of Dependent Packages
======
```r
# CRAN
install.packages(c("XXXXX"),
    repos="http://cran.r-project.org")

# Bioconductor
library("BiocManager")
BiocManager::install(c("XXXXX"),
    suppressUpdates=TRUE)
```

Installation
======
```r
git clone https://github.com/rikenbit/DelayedTensor/
R CMD INSTALL DelayedTensor
```
or type the code below in the R console window
```r
install.packages("devtools", repos="http://cran.r-project.org")
library(devtools)
devtools::install_github("rikenbit/DelayedTensor")
```

## License
Copyright (c) 2021 Koki Tsuyuzaki and Laboratory for Bioinformatics Research, RIKEN Center for Biosystems Dynamics Reseach
Released under the [Artistic License 2.0](http://www.perlfoundation.org/artistic_license_2_0).

## Authors
- Koki Tsuyuzaki
- Itoshi Nikaido