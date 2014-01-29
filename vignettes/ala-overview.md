# ALA4R

This is a project to enable the R community to access data and resources hosted at Atlas of Living Australia (ALA). The goal is to enable outputs (e.g., observations of species) to be queried and output in standard formats.

## Installing

First, install the package dependencies from CRAN:


```r
install.packages(c("httr", "rjson", "stringr", "plyr", "digest", "RCurl", "rgeos"))
```


Then install the ALA4R package itself from RForge:

```r
install.packages("ALA4R", repos = "http://rforge.net/")
```


To actually use the package it needs to be loaded. This needs to be done in each new R session:


```r
library(ALA4R)
```


## Customizing

Some details of how ALA4R operates can be customized.

### Caching
ALA4R can cache most results to local files. This means that if the same code is run multiple times, the second and subsequent iterations will be faster (and reduce load on the ALA servers). By default, this caching is session-based, meaning that the local files are stored in a temporary directory that is automatically deleted when the R session is ended. This can be altered so that caching is permanent, by setting the caching directory to a non-temporary location, e.g.:


```r
ala_config(cache_directory = file.path("c:", "mydata", "ala_cache"))  ## Windows
```


or 


```r
ala_config(cache_directory = file.path("~", "mydata", "ala_cache"))  ## Linux
```


Note that this directory must exist (you need to create it yourself).

This means that results will be stored in that directory and used from one session to the next. They won't be re-downloaded from the server unless the user deletes those files or changes the caching setting to "refresh".

If you change the cache_directory to a permanent location, you may wish to add something like this to your .Rprofile file, so that it happens automatically each time the ALA4R package is loaded:


```r
setHook(packageEvent("ALA4R", "attach"), function(...) ala_config(cache_directory = file.path("~", 
    "mydata", "ala_cache")))
```


Caching can also be turned off entirely:


```r
ala_config(caching = "off")
```


or set to "refresh", meaning that the cached results will re-downloaded from the ALA servers.

### User-agent string
Each request to the ALA servers is accompanied by a "user-agent" string that identifies the software making the request. (This is standard behaviour, browsers do this as well.) By default, the ALA4R user-agent string is set to "ALA4R" plus the ALA4R version number, R version, and operating system (e.g. "ALA4R 0.16 (R version 3.0.2 (2013-09-25)/x86_64-pc-linux-gnu)"). Note that no personally identifying information is sent. You can see the user-agent string with:


```r
ala_config()
```


In a future release, we will add a function that will allow you to supplemented the user-agent string with information about your usage of ALA data, should you wish to do so. This will help the ALA keep track of which broad user communities are using what data.

### Debugging
If things aren't working as expected, more detail (particularly about web requests and caching behaviour) can be obtained by setting:


```r
ala_config(verbose = TRUE)
```

