# ALA4R

The Atlas of Living Australia (ALA) provides tools to enable users of biodiversity information to find, access, combine and visualise data on Australian plants and animals; these have been made available from http://www.ala.org.au/. Here we provide a subset of the tools to be directly used within R.

Currently in a very preliminary state: everything is liable to change.

## Installing

Binaries are available from RForge for easy installation in R. Install the dependencies first:

```{r}
install.packages(c("httr","stringr","plyr","digest","RCurl","rgeos","jsonlite"))
```

Then the ALA4R package itself:

```{r}
install.packages("ALA4R",repos="http://rforge.net/")
```

Under windows, you may need to replace the second command with:

```{r}
install.packages("ALA4R",repos="http://rforge.net/",type="source")
```


