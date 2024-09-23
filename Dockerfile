# Base Image
FROM bioconductor/bioconductor_docker:devel

RUN apt-get update \
    && apt-get install -y --no-install-recommends apt-utils \
    time \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    ## Install Packages
    && /usr/local/bin/R -e 'BiocManager::install(version = "devel", ask=FALSE); BiocManager::valid(); BiocManager::install(c("DelayedRandomArray", "HDF5Array"), ask=FALSE, force=TRUE); remotes::install_github("rikenbit/DelayedTensor", force=TRUE, upgrade="always", INSTALL_opts = '--install-tests'); tools::testInstalledPackage('DelayedTensor'); library("DelayedRandomArray"); library("HDF5Array"); library("DelayedTensor")'
