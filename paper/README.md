# Combined P-value Functions for Compatible Effect Estimation and Hypothesis Testing in Drug Regulation

This directory contains code and data related to the preprint

> Pawel, S., Roos, M., Held. L. (2025). Combined *P*-value Functions for Compatible Effect Estimation and Hypothesis Testing in Drug Regulation. <https://doi.org/10.48550/arXiv.2503.10246>

## Reproducibility

The results can be reproduced by installing the necessary R packages

``` r
## CRAN packages
pkgs <- c("remotes", "knitr", "ggplot2", "dplyr", "kableExtra", "twotrials")
install.packages(pkgs)

## GitHub packages
remotes::install_github("felix-hof/confMeta")
```

and then rerunning the code in `paper/est2TR.R`. To recompile the manuscript
make sure to have LaTeX installed (tested only with TeX Live 2023/Debian) and the Wiley NJD LaTeX template [files](https://authorservices.wiley.com/author-resources/Journal-Authors/Prepare/new-journal-design.html) in the `paper/` directory. Then run

``` sh
make
```

which should produce `paper/est2TR.pdf`. The R and R package versions that were
used when the paper was successfully compiled before submission are visible in
the following output


``` r
sessionInfo()

#> R version 4.4.1 (2024-06-14)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.2 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0 
#> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=de_CH.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=de_CH.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=de_CH.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=de_CH.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Europe/Zurich
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] dplyr_1.1.4      kableExtra_1.4.0 twotrials_0.6    ggplot2_3.5.2    confMeta_0.4.2   knitr_1.48      
#> 
#> loaded via a namespace (and not attached):
#>  [1] generics_0.1.3           xml2_1.3.6               stringi_1.8.4            lattice_0.22-6          
#>  [5] lme4_1.1-36              hms_1.1.3                digest_0.6.37            magrittr_2.0.3          
#>  [9] evaluate_0.24.0          grid_4.4.1               meta_8.0-2               fastmap_1.2.0           
#> [13] CompQuadForm_1.4.3       Matrix_1.7-2             purrr_1.0.4              viridisLite_0.4.2       
#> [17] scales_1.3.0             numDeriv_2016.8-1.1      reformulas_0.4.0         Rdpack_2.6.2            
#> [21] cli_3.6.4                rlang_1.1.5              rbibutils_2.3            ReplicationSuccess_1.3.3
#> [25] munsell_0.5.1            splines_4.4.1            withr_3.0.2              tools_4.4.1             
#> [29] tzdb_0.4.0               nloptr_2.1.1             minqa_1.2.8              metafor_4.8-0           
#> [33] colorspace_2.1-1         mathjaxr_1.6-0           boot_1.3-30              vctrs_0.6.5             
#> [37] R6_2.6.1                 lifecycle_1.0.4          stringr_1.5.1            MASS_7.3-64             
#> [41] pkgconfig_2.0.3          pillar_1.10.1            gtable_0.3.6             glue_1.8.0              
#> [45] Rcpp_1.0.14              systemfonts_1.1.0        xfun_0.49                tibble_3.2.1            
#> [49] tidyselect_1.2.1         rstudioapi_0.16.0        farver_2.1.2             htmltools_0.5.8.1       
#> [53] nlme_3.1-167             patchwork_1.3.0          labeling_0.4.3           svglite_2.1.3           
#> [57] rmarkdown_2.27           readr_2.1.5              compiler_4.4.1           metadat_1.5-0 
```
