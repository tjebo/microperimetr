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
testdata <- read_maia(folder = file.path(getwd(), "norm_raw"))
comparedat <- compare_norm(testdata)
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
