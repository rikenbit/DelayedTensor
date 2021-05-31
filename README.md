# DelayedTensor
Block-processed version of rTensor functions to perform tensor arithmetic and decomposition.

All the calculations are actually performed and not registered as delayed operartion.

DelayedTensor provides some generic function related to Tensor arithmetic/decompotision and dispatches it on the DelayedArray class.

DelayedTensor also suppors Tensor contraction by einsum function, which is inspired by numpy einsum.

Installation of Dependent Packages
======
```r
library("BiocManager")
BiocManager::install(
  c("DelayedArray", "HDF5Array", "BiocSingular", "rTensor",
    "DelayedRandomArray", "einsum", "BiocStyle", "knitr",
    "markdown", "rmarkdown", "testthat", "magrittr", "dplyr"),
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