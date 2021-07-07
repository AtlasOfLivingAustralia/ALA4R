
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ALA4R

[![Travis-CI Build
Status](https://travis-ci.org/AtlasOfLivingAustralia/ALA4R.svg?branch=master)](https://travis-ci.org/AtlasOfLivingAustralia/ALA4R)
[![AppVeyor Build
status](https://ci.appveyor.com/api/projects/status/g9pudc4l7053w4vn/branch/master?svg=true)](https://ci.appveyor.com/project/PeggyNewman/ala4r/branch/master)
[![codecov](https://codecov.io/gh/AtlasOfLivingAustralia/ALA4R/branch/master/graph/badge.svg)](https://codecov.io/gh/AtlasOfLivingAustralia/ALA4R)
[![CRAN
Status](https://www.r-pkg.org/badges/version/ALA4R)](https://www.r-pkg.org/pkg/ALA4R)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ALA4R)](https://cran.r-project.org/package=ALA4R)

*`ALA4R` is deprecated and will be removed from CRAN at the end of 2021.
The package to replace ALA4R, `galah`, is now [available on
CRAN](https://CRAN.R-project.org/package=galah). `galah` provides an
improved interface to ALA data, while providing the same core
functionality as ALA4R. For an introduction to `galah`, visit the
[GitHub page](https://github.com/AtlasOfLivingAustralia/galah).*

The Atlas of Living Australia (ALA) provides tools to enable users of
biodiversity information to find, access, combine and visualise data on
Australian plants and animals; these have been made available from
<https://www.ala.org.au/>. Here we provide a subset of the tools to be
directly used within R.

ALA4R enables the R community to directly access data and resources
hosted by the ALA.

The use-examples presented at the [2014 ALA Science
Symposium](https://www.ala.org.au/blogs-news/2014-atlas-of-living-australia-science-symposium/)
are available in the package vignette, via (in R): `vignette("ALA4R")`,
or [browse it
online](https://atlasoflivingaustralia.github.io/ALA4R/articles/ALA4R.html).

## Citing ALA4R

To generate a citation for ALA4R run:

``` r
citation(package = "ALA4R")
```

## Installing

Stable version from CRAN:

``` r
install.packages("ALA4R")
```

Or the development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("AtlasOfLivingAustralia/ALA4R")
```

On Linux you will first need to ensure that `libcurl` and `v8` (version
&lt;= 3.15) are installed on your system — e.g. on Ubuntu/Debian, open a
terminal and do:

``` sh
sudo apt-get install libcurl4-openssl-dev libv8-3.14-dev
```

or install via the Software Centre.

## Usage

See the online documentation at
<https://atlasoflivingaustralia.github.io/ALA4R/> including the [package
vignette](https://atlasoflivingaustralia.github.io/ALA4R/articles/ALA4R.html).
