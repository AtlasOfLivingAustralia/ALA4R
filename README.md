# ALA4R

The Atlas of Living Australia (ALA) provides tools to enable users of biodiversity information to find, access, combine and visualise data on Australian plants and animals; these have been made available from http://www.ala.org.au/. Here we provide a subset of the tools to be directly used within R.

The use-examples presented at the [2014 ALA Science Symposium](http://www.ala.org.au/blogs-news/2014-atlas-of-living-australia-science-symposium/) are available in the package vignette, via (in R): `vignette("ALA4R")` and also [as a pdf here](./vignettes/ALA4R.pdf?raw=true).


## Installing

### Windows

In R:

```{r}
install.packages("devtools")
library(devtools)
install_github("AtlasOfLivingAustralia/ALA4R")
```

You may see a warning about the `Rtools` package: you don't need to install this. You may also be asked about a location for the `R.cache` directory --- choose whatever you prefer here, ALA4R does not use `R.cache`.

If you wish to use the `data.table` package for potentially faster loading of data matrices (optional), also do:
```{r}
install.packages("data.table")
```


### Linux

First, ensure that `libcurl` is installed on your system --- e.g. on Ubuntu, open a terminal and do:
```
sudo apt-get install libcurl4-openssl-dev
```

or install `libcurl4-openssl-dev` via the Software Centre.


Then follow the instructions for Windows, above.

