# ALA4R

The Atlas of Living Australia (ALA) provides tools to enable users of biodiversity information to find, access, combine and visualise data on Australian plants and animals; these have been made available from http://www.ala.org.au/. Here we provide a subset of the tools to be directly used within R.

Currently in a very preliminary state: everything is liable to change.

## Installing

Binaries will be available from RForge for easy installation in R: 

```s
install.packages(c("httr","rjson","stringr","plyr","digest","RCurl","rgeos")) ## install dependencies
install.packages("ALA4R",repos="http://rforge.net/")
```

