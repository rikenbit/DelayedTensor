# Base Image
FROM bioconductor/bioconductor_docker:RELEASE_3_19

# Install R Packages
RUN R -e "BiocManager::install(version = '3.19', ask=FALSE); BiocManager::valid(); BiocManager::install('DelayedRandomArray', ask=FALSE); devtools::install_github('rikenbit/DelayedTensor', \
    upgrade='always', force=TRUE, INSTALL_opts = '--install-tests');\
    tools::testInstalledPackage('DelayedTensor')"
