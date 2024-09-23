# Base Image
FROM bioconductor/bioconductor_docker:devel

# Install R Packages
RUN R -e "BiocManager::install(version = 'devel', ask=FALSE); BiocManager::valid(); BiocManager::install('DelayedRandomArray', ask=FALSE, force=TRUE); remotes::install_github('rikenbit/DelayedTensor', force=TRUE, dependencies=TRUE, upgrade="always", INSTALL_opts = '--install-tests'); tools::testInstalledPackage('DelayedTensor')"
