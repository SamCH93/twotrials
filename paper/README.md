# Combined P-value Functions for Compatible Effect Estimation and Hypothesis Testing in Drug Regulation

This directory contains code and data related to the preprint

> Pawel, S., Roos, M., Held. L. (2025). Combined P-value Functions for Compatible Effect Estimation and Hypothesis Testing in Drug Regulation.

## Reproducibility

The results can be reproduced by installing the necessary R packages

``` r
## CRAN packages
pkgs <- c("xtable", "remotes", "knitr", "ggplot2", "dplyr", "kableExtra")
install.packages(pkgs)

## GitHub packages
remotes::install_github("SamCH93/twotrials", subdir = "package")
remotes::install_github("felix-hof/confMeta")
```

and then rerunning the code in `paper/est2TR.R`. To recompile the manuscript make
sure to have LaTeX installed (tested only with TeX Live 2023/Debian) and
then run

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
#> Running under: Ubuntu 22.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Etc/UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] dplyr_1.1.4      kableExtra_1.4.0 twotrials_0.05   ggplot2_3.5.1    confMeta_0.4.2   xtable_1.8-4    
#> [7] knitr_1.48      
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.4               generics_0.1.3           xml2_1.3.6               stringi_1.8.4           
#>  [5] lattice_0.22-6           digest_0.6.37            lme4_1.1-35.5            hms_1.1.3               
#>  [9] magrittr_2.0.3           evaluate_1.0.1           grid_4.4.1               meta_7.0-0              
#> [13] fastmap_1.2.0            CompQuadForm_1.4.3       Matrix_1.7-0             purrr_1.0.2             
#> [17] fansi_1.0.6              viridisLite_0.4.2        scales_1.3.0             numDeriv_2016.8-1.1     
#> [21] cli_3.6.3                rlang_1.1.4              ReplicationSuccess_1.3.3 munsell_0.5.1           
#> [25] splines_4.4.1            withr_3.0.2              tools_4.4.1              tzdb_0.4.0              
#> [29] nloptr_2.1.1             minqa_1.2.8              metafor_4.6-0            colorspace_2.1-1        
#> [33] mathjaxr_1.6-0           boot_1.3-30              vctrs_0.6.5              R6_2.5.1                
#> [37] lifecycle_1.0.4          stringr_1.5.1            MASS_7.3-60.2            pkgconfig_2.0.3         
#> [41] pillar_1.9.0             gtable_0.3.6             glue_1.8.0               Rcpp_1.0.13             
#> [45] systemfonts_1.1.0        xfun_0.48                tibble_3.2.1             tidyselect_1.2.1        
#> [49] rstudioapi_0.17.1        farver_2.1.2             htmltools_0.5.8.1        nlme_3.1-164            
#> [53] patchwork_1.3.0          labeling_0.4.3           svglite_2.1.3            rmarkdown_2.28          
#> [57] readr_2.1.5              compiler_4.4.1           metadat_1.2-0  
```
