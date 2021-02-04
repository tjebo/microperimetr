<!-- README.md is generated from README.Rmd. Please edit that file -->

# microperimetr

This package will help you work with microperimetry data.  
It is built for centervue’s maia and compass devices.

## Features

  - Extracting data from MAIA raw (tgz) files becomes as easy as pie.
  - Fully automated comparison of your visual field data with normdata
    from the literature.
  - microperimetr has functions for both location-dependent and
    location-independent visual field analysis. Because the normdata is
    interpolated for location-dependent analysis, any custom grid can be
    used for both types of analysis.
  - microperimetr also includes functions for some more or less common
    visualisation of visual field data

## Examples

This is a basic example which shows you how to import your visual field
data and plot the (estimated) mean deviation for each included test:

### Load sample visual field data in a breeze

``` r
# save the tgz files from MAIA patient backup into any directory, 
# Here this is "data_raw", a folder in the working directory 
library(microperimetr)
# read the tgz files
testdata <- read_maia_tgz(folder = file.path(getwd(), "data-raw"))
#Interpolate normal values for the plot 
compare_dat <- compare(testdata)
```

### Get some basic visual field statistics

``` r
# Visual fields statistics arranged nicely in a data frame
mpstats(compare_dat)
#>              mean_sens sd_sens mean_dev  psd
#> 2962_mesopic     23.92    3.61    -1.01 3.02
#> 2963_mesopic     23.94    3.72    -0.97 2.96
#> 2964_cyan        19.06    4.65    -0.96 4.44
#> 2964_red         20.65    4.54    -1.34 3.32
#> 2965_cyan        19.65    4.51    -0.48 4.52
#> 2965_red         19.65    4.51    -2.40 4.33
#> 2993_cyan        14.20    7.57    -4.88 9.41
#> 2994_mesopic     24.21    3.71    -0.39 3.26
#> 2995_mesopic     23.36    3.67    -1.30 3.16
#> 2996_cyan        22.08    4.61     2.42 4.20
#> 2996_red         21.54    3.48    -0.30 2.96
#> 2997_cyan        21.35    4.05     1.49 4.31
#> 2997_red         21.35    4.05    -0.50 4.02
#> 3016_mesopic     25.88    4.14     1.33 3.67
#> 3017_mesopic     25.64    3.68     1.11 3.15
#> 3018_cyan        19.85    4.48     0.42 3.66
#> 3018_red         22.35    3.11     0.51 2.67
#> 3019_cyan        21.02    4.62     1.31 4.33
#> 3019_red         21.02    4.62    -0.87 4.82
#> 3024_mesopic     23.94    3.71    -0.59 3.09
#> 3025_mesopic     24.06    3.68    -0.38 3.02
#> 3026_cyan        18.56    5.03    -0.57 3.26
#> 3026_red         22.85    3.43     1.10 2.79
#> 3027_cyan        20.26    4.86     0.78 4.32
#> 3027_red         20.26    4.86    -1.51 4.96
#> 33_mesopic       25.87    3.97    -0.51 3.66
#> 34_mesopic       25.00    4.03    -1.36 3.79
#> 35_cyan          10.10    7.19   -11.36 6.17
#> 35_red           10.38    3.04   -12.95 3.42
#> 36_cyan           8.75    7.14   -12.73 6.30
#> 36_red           11.15    3.08   -12.17 3.36
#> 37_cyan           7.56    6.82   -14.12 6.14
#> 834_mesopic      26.50    4.24     0.16 3.97
#> 835_mesopic      28.46    4.73     2.18 4.52
```

### Plot bebie curves

``` r
# prepare the data
field_var <- field_variation(testdata)
bebie_stats <- calc_bebie(testdata, field_var)
```

``` r
plot_bebie(bebie_stats)
```

### Plot other visual field statistics

``` r
# Plotting mean sensitivity, mean deviation and pattern standard deviation
# Shows the visual field statistics in relation to normal percentiles. 
plot_mpstats(compare_dat)
```

![](README-plot_mpstats-1.png)<!-- -->

## Installation

``` r
# for the development version 
devtools::install_github(tjebo/microperimetr)
```

## Sources

Norm data was used from Jonathan Denniss and Maximilian Pfau.

[Raw data from Denniss et al. has been made available
online](https://www.sciencedirect.com/science/article/pii/S2352340916304978).

Accompanying paper in IOVS:
<http://iovs.arvojournals.org/article.aspx?articleid=2571342>
