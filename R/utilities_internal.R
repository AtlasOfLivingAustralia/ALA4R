## some utility functions used internally within the ALA4R library: not exported 

##----------------------------------------------------------------------------------------------

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

##----------------------------------------------------------------------------------------------

clean_string <- function(x) {
	x = gsub("[^[:alpha:]\\. ]", "", x) #remove anything but alpha characters
	x = str_trim(x) ## remove leading and trailing whitespaces
	x = gsub('\\s+',' ',x) ## replace multiple whitespaces with single
	x
}

##----------------------------------------------------------------------------------------------

##convert to camel case ... modified from help forum example
## not exported for users: internal ALA4R use only
tocamel = function (x, delim = "[^[:alnum:]]", upper = FALSE, sep = "") {
    assert_that(is.character(x))
    assert_that(is.string(delim))
    s <- strsplit(x, delim)
	tfun = function(y) {
        if (any(is.na(y))) {
            y
        }
        else {
            first <- substring(y, 1, 1)
            if (isTRUE(upper)) 
                first <- toupper(first)
            else first[-1] <- toupper(first[-1])
            paste(first, substring(y, 2), sep = "", collapse = sep)
        }
    }
    sapply(s, tfun)
}

##----------------------------------------------------------------------------------------------

## define column names that we will remove from the results because we don't think they will be useful in the ALA4R context
unwanted_columns=function(type) {
    type=match.arg(tolower(type),c("general","layers","occurrence","assertions"))
    switch(type,
           "general"=c("rawRank","rawRankString","rankId","rankID","left","right","idxType","highlight","linkIdentifier","isExcluded"),
             ## rawRank appears to be a duplicate of rank or rankString
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

##----------------------------------------------------------------------------------------------

rename_variables=function(varnames,type,verbose=ala_config()$verbose) {
    if (length(varnames)<1) {
        ## catch in case names from empty data frame were passed
        return(varnames)
    }
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

        ## ** TODO more exhaustively**
        for (kw in c("longitude","latitude","value","units")) {
            varnames=str_replace_all(varnames,kw,paste(toupper(substring(kw,1,1)),substring(kw,2),sep=""))
        }
        ## some that only seem to appear at the ends of variable names, so be conservative with these replacements
        for (kw in c("min","max","path")) {
            varnames=str_replace_all(varnames,paste(kw,"$",sep=""),paste(toupper(substr(kw,1,1)),substring(kw,2),sep=""))
        }        
        ## enforce first letter lowercase
        varnames=paste(tolower(substr(varnames,1,1)),substring(varnames,2),sep="")
        if (type %in% c("layers","occurrence")) {
            ## but some acronyms in layer names should remain all-uppercase
            ## currently this list is: c("iBRA","iMCRA","aCTTAMS","gER","nZ","nSW","lGA","nRM","rAMSAR","nDVI","nPP","aSRI","gEOMACS")
            ## but since these all occur at the start of variable names, we can catch them with a regular expression and not need to hard-code a list
            idx=str_detect(varnames,"^[a-z][A-Z]")
            temp=varnames[idx]
            varnames[idx]=paste(toupper(substr(temp,1,1)),substring(temp,2),sep="")
            ## "seaWIFS" to "SeaWIFS"
            varnames=str_replace_all(varnames,"seaWIFS","SeaWIFS")
        }
        ## some global re-naming
        if (type=="general") {
            ## general names, from e.g. name searching
            varnames[varnames=="occCount"]="occurrenceCount"
            varnames[varnames=="vernacularName"]="commonName" ## taxinfo_download provides "vernacularName", others "commonName"
            varnames=str_replace_all(varnames,"conservationStatusIn","conservationStatus")
            varnames=str_replace_all(varnames,"scientificNameForAcceptedConcept","acceptedConceptName") ## taxinfo_download returns the former, but should be the latter for consistency elsewhere

            if (any(varnames=="rank") & any(varnames=="rankString")) {
                if (verbose) {
                    warning("data contains both \"rank\" and \"rankString\" columns, not renaming \"rankString\"")
                }
            } else {
                varnames[varnames=="rankString"]="rank" ## returned as "rank" by some services and "rankString" by others
            }
            ## ditto for taxonRank
            if (any(varnames=="rank") & any(varnames=="taxonRank")) {
                if (verbose) {
                    warning("data contains both \"rank\" and \"taxonRank\" columns, not renaming \"taxonRank\"")
                }
            } else {
                varnames[varnames=="taxonRank"]="rank" ## returned as "Taxon.Rank" (camelcased to "taxonRank") by taxinfo_download
            }
            
            
        } else if (type=="layers") {
            varnames[varnames=="desc"]="description"
        } else if (type=="occurrence") {
        } else if (type=="assertions") {
        }
    }
    varnames
}
