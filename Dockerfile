# Base Image
FROM bioconductor/bioconductor_docker:devel

# Install R Packages
RUN R -e "BiocManager::install(version = 'devel', ask=FALSE); BiocManager::valid(); BiocManager::install('HDF5Array', 'DelayedRandomArray', ask=FALSE); devtools::install_github('rikenbit/DelayedTensor', \
    upgrade='always', force=TRUE, INSTALL_opts = '--install-tests');\
    tools::testInstalledPackage('DelayedTensor')"
