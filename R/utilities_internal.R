## some utility functions used internally within the ALA4R library: not exported 

## internal function for converting chr data types to numeric or logical
convert_dt=function(x,test_numeric=TRUE) {
    ## set test_numeric to FALSE to skip checking for numeric columns - might be a little faster if not needed
    assert_that(is.flag(test_numeric))
    if (see_if(is.character(x))) {
        ux=unique(x)
        if (all(nchar(ux)<1)) {
            ## all empty strings - leave as is
        } else if (all(ux %in% c("true","false","TRUE","FALSE","","NA"))) {
            x=as.logical(x)
        } else if (test_numeric) {
            if (all(nchar(ux)<1 | ux=="NA" | !is.na(suppressWarnings(as.numeric(ux))))) {
                x=as.numeric(x)
            }
        }
    }
    x
}


clean_string <- function(x) {
	x = gsub("[^[:alpha:]\\. ]", "", x) #remove anything but alpha characters
	x = str_trim(x) ## remove leading and trailing whitespaces
	x = gsub('\\s+',' ',x) ## replace multiple whitespaces with single
	x
}


## define column names that we will remove from the results because we don't think they will be useful in the ALA4R context
unwanted_columns=function(type) {
    type=match.arg(tolower(type),c("general","layers","occurrence","assertions"))
    switch(type,
           "general"=c("rawRank","rankId","left","right","idxType","highlight","linkIdentifier","isExcluded","hasChildren","image","thumbnail"),
             ## rawRank appears to be a duplicate of rank or rankString
             ## hasChildren seems always to be false, even for taxa that ought to have children (e.g. Macropus)
             ## image and thumbnail appear to be internal paths, not full URLs
           "layers"=c("pid","path","path_orig","path_1km","enabled","uid","licence_level","lookuptablepath","mdhrlv","mddatest","datalang","grid","shape","enabled","indb","spid","sid","sdesc","sname"),
             ## datalang appears to be all "eng" "Eng" "enu" "" or NA (2x"enu" records appear to be in English and from DEH/DEWHA)
             ## grid is redundant: all env layers are grid==TRUE, all contextual layers are grid==NA
             ## ditto for shape: all contextual are TRUE, all grid are NA
             ## mddatest is an internal metadata testing date of some sort?
             ## enabled appears to be all TRUE
             ## spid is redundant with id
             ## no idea what sid,sname, or sdesc are, but don't look particularly useful in our context
           c("")
           )
}


rename_variables=function(varnames,type,verbose=ala_config()$verbose) {
    assert_that(is.character(varnames))
    assert_that(is.string(type))
    type=match.arg(tolower(type),c("general","layers","occurrence","assertions","other")) ## use "other" to make no variable name substtutions, just enforce case/separator conventions
    if (FALSE) {
        ## just return the names as-is, but enforce validity as variable names
        varnames=make.names(varnames)
     } else {
        ## change all to camelCase
        varnames=tocamel(make.names(varnames))
        ## try to convert some all-lowercase names to camel, e.g. environmentalvaluemax minlatitude minlongitude

        ## ** todo **
        
        ## enforce first letter lowercase
        varnames=paste(tolower(substr(varnames,1,1)),substring(varnames,2),sep="")
        ## some global re-naming
        if (type=="general") {
            ## general names, from e.g. name searching
            varnames[varnames=="occCount"]="occurrenceCount"
            if (any(varnames=="rank") & any(varnames=="rank")) {
                if (verbose) {
                    warning("data contains both \"rank\" and \"rankString\" columns, not renaming \"rankString\"")
                }
            } else {
                varnames[varnames=="rankString"]="rank" ## returned as "rank" by some services and "rankString" by others
            }
        } else if (type=="layers") {
        } else if (type=="occurrence") {
        } else if (type=="assertions") {
        }
    }
    varnames
}
