## set R version (https://hub.docker.com/r/rocker/verse/tags)
FROM rocker/verse:4.4.1

## set up directories
WORKDIR home/rstudio
RUN mkdir /home/rstudio/paper
COPY package /home/rstudio/package

## install R packages from CRAN the last day of the specified R version
RUN install2.r --error --skipinstalled --ncpus -1 \
    xtable remotes knitr ggplot2 dplyr kableExtra meta metafor patchwork ReplicationSuccess && \
    R -e "remotes::install_github('felix-hof/confMeta@6ce40a4b581e76eee30a0d02b8c0c8ba77db51b0', upgrade_dependencies = FALSE)" && \
    R CMD INSTALL  package/out/twotrials_0.05.tar.gz
