<!-- README.md is generated from README.Rmd. Please edit that file -->

# microperimetr

This package will help you work with microperimetry data.  
It is built for centervue’s maia and compass devices. (note: currently
only for maia device!)

## Features

- Extracting data from maia raw (tgz) files becomes as easy as pie.
- contains normdata from literature.
- microperimetr (will) work hand in hand with the perimetry package that
  will have functions for both location-dependent and
  location-independent visual field analysis. Because the normdata is
  interpolated for location-dependent analysis, any custom grid can be
  used for both types of analysis.
- microperimetr also includes functions for some more or less common
  visualisation of visual field data

## Examples

This is a basic example which shows you how to import your visual field
data and plot the (estimated) mean deviation for each included test:

### Load sample visual field data in a breeze

    # save the tgz files from MAIA patient backup into any directory, 
    # Here this is "data_raw", a folder in the working directory 
    library(microperimetr)
    # read the tgz files
    testdata <- read_maia_tgz()

## Installation

    # for the development version 
    devtools::install_github(tjebo/microperimetr)

## Sources

Norm data was used from Jonathan Denniss and Maximilian Pfau.

[Raw data from Denniss et al. has been made available
online](https://www.sciencedirect.com/science/article/pii/S2352340916304978).

Accompanying paper in IOVS:
<http://iovs.arvojournals.org/article.aspx?articleid=2571342>
