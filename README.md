# ALA4R

The Atlas of Living Australia (ALA) provides tools to enable users of biodiversity information to find, access, combine and visualise data on Australian plants and animals; these have been made available from http://www.ala.org.au/. Here we provide a subset of the tools to be directly used within R.

The use-examples presented at the [2014 ALA Science Symposium](http://www.ala.org.au/blogs-news/2014-atlas-of-living-australia-science-symposium/) are available in the package vignette, via (in R): `vignette("ALA4R")` and also [as a pdf here](./vignettes/ALA4R.pdf?raw=true).


## Installing

### Stable version

Most users will want this.

#### Windows

In R:

```{r}
install.packages("ALA4R")
```

If you wish to use the `data.table` package for potentially faster loading of data matrices (optional), also do:
```{r}
install.packages("data.table")
```



#### Linux

First, ensure that `libcurl` is installed on your system --- e.g. on Ubuntu, open a terminal and do:
```
sudo apt-get install libcurl4-openssl-dev
```

or install `libcurl4-openssl-dev` via the Software Centre.

Then, in R:

```{r}
install.packages("ALA4R")
```

If you wish to use the `data.table` package for potentially faster loading of data matrices (optional), also do:
```{r}
install.packages("data.table")
```

### Development version

The development version is hosted on rforge.net. It is more up-to-date than the stable version, and may have additional functionality, but may also have unresolved issues.

#### Windows

Install the dependencies first from CRAN:
```{r}
install.packages(c("httr","stringr","plyr","digest","RCurl","jsonlite","assertthat","sp"))
```

If you wish to use the `data.table` package for potentially faster loading of data matrices (optional), also do:
```{r}
install.packages("data.table")
```

Then the ALA4R package itself:
```{r}
install.packages("ALA4R",repos="http://rforge.net/",type="source")
```

#### Linux

First, ensure that `libcurl` is installed on your system --- e.g. on Ubuntu, open a terminal and do:
```
sudo apt-get install libcurl4-openssl-dev
```

or install `libcurl4-openssl-dev` via the Software Centre.

Then, fire up R and install the dependencies from CRAN:
```{r}
install.packages(c("httr","stringr","plyr","digest","RCurl","jsonlite","assertthat","sp"))
```

If you wish to use the `data.table` package for potentially faster loading of data matrices (optional), also do:
```{r}
install.packages("data.table")
```

Then the ALA4R package itself:
```{r}
install.packages("ALA4R",repos="http://rforge.net/")
```

