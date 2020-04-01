<!-- README.md is generated from README.Rmd. Please edit that file -->
microperimetR
=============

This package will help you work with microperimetry data.  
It is built for centervue’s maia and compass devices.

Features:
---------

-   Extracting data from MAIA raw (tgz) files becomes as easy as pie.
-   Fully automated comparison of your visual field data with normdata
    from the literature.
-   microperimetR has functions for both location-dependent and
    location-independent visual field analysis. Because the normdata is
    interpolated for location-dependent analysis, any custom grid can be
    used for both types of analysis.
-   microperimetR also includes functions for some more or less common
    visualistaion of visual field data

Example
-------

This is a basic example which shows you how to import your visual field
data and plot the (estimated) mean deviation for each included test:

``` r
# save the tgz files from MAIA into your R working directory 
# devtools::install_github(tjebo/microperimetR)
library(microperimetR)
#> Loading required package: tidyverse
#> ── Attaching packages ──────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
#> ✓ tibble  2.1.3     ✓ dplyr   0.8.5
#> ✓ tidyr   1.0.2     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> ── Conflicts ─────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
testdata <- read_maia(folder = file.path(getwd(), "norm_raw"))
#> Loading required package: xml2
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following object is masked from 'package:base':
#> 
#>     date
#> Loading required package: jsonlite
#> 
#> Attaching package: 'jsonlite'
#> The following object is masked from 'package:purrr':
#> 
#>     flatten
comparedat <- compare_norm(testdata)
#> Loading required package: gstat
#> Loading required package: sp
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
plot_MD(comparedat)
```

![](README-example-1.png)
