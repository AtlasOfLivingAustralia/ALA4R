# ALA4R

The Atlas of Living Australia (ALA) provides tools to enable users of biodiversity information to find, access, combine and visualise data on Australian plants and animals; these have been made available from http://www.ala.org.au/. Here we provide a subset of the tools to be directly used within R.

Currently in a very preliminary state: everything is liable to change.

## Installing

### Windows

Install the dependencies first:
```{r}
install.packages(c("httr","stringr","plyr","digest","RCurl","jsonlite","assertthat"))
```

If you wish to use the `sp` package for spatial data and `data.table` package for potentially faster loading of data matrices (both optional), also do:
```{r}
install.packages(c("sp","data.table"))
```

Then the ALA4R package itself:
```{r}
install.packages("ALA4R",repos="http://rforge.net/",type="source")
```

### Linux

First, ensure that `libcurl` is installed on your system --- e.g. on Ubuntu, open a terminal and do:
```
sudo apt-get install libcurl4-openssl-dev
```

or install `libcurl4-openssl-dev` via the Software Centre.

Then, fire up R and install the dependencies that we need:
```{r}
install.packages(c("httr","stringr","plyr","digest","RCurl","jsonlite","assertthat"))
```

If you wish to use the `sp` package for spatial data and `data.table` package for potentially faster loading of data matrices (both optional), also do:
```{r}
install.packages(c("sp","data.table"))
```

Then the ALA4R package itself:
```{r}
install.packages("ALA4R",repos="http://rforge.net/")
```

