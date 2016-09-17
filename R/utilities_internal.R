## some utility functions used internally within the ALA4R library: not exported 

##----------------------------------------------------------------------------------------------

is.notempty.string <- function(x) {
    is.string(x) && !is.na(x) && nchar(x)>0
}

##----------------------------------------------------------------------------------------------

## internal function for converting chr data types to numeric or logical
convert_dt <- function(x,test_numeric=TRUE) {
    ## set test_numeric to FALSE to skip checking for numeric columns - might be a little faster if not needed
    assert_that(is.flag(test_numeric))
    if (see_if(is.character(x))) {
        ux <- unique(x)
        ## non-valid encoding of strings here will cause failure
        encoding_ok <- function(s) {
            temp <- try({nchar(s); TRUE}, silent=TRUE) ## will be TRUE if successful, or an error message if not
            is.logical(temp) && temp
        }
        if (!encoding_ok(ux)) {
            x <- enc2utf8(x) ## force to utf8
            ux <- unique(x)
        }
        if (all(nchar(ux)<1)) {
            ## all empty strings - leave as is
        } else if (all(ux %in% c("true","false","TRUE","FALSE","","NA"))) {
            x <- as.logical(x)
        } else if (test_numeric) {
            if (all(nchar(ux)<1 | ux=="NA" | !is.na(suppressWarnings(as.numeric(ux))))) {
                x <- as.numeric(x)
            }
        }
    }
    x
}

##----------------------------------------------------------------------------------------------

clean_string <- function(x) {
    ## only used in search_names and search_partial_name

    ##x <- gsub("[^[:alpha:]\\. ]", "", x) #remove anything but alpha characters
    ## removed this: scientific names can't contain hyphens or diacriticals but common names can; removing these
    ## characters causes problems with hyphenated names and seems likely not to behave well with internationalisation anyway
    x <- str_trim(x) ## remove leading and trailing whitespaces
    gsub('\\s+',' ',x) ## replace multiple whitespaces with single
}

##----------------------------------------------------------------------------------------------

##convert to camel case ... modified from help forum example
## not exported for users: internal ALA4R use only
tocamel <- function (x, delim = "[^[:alnum:]]", upper = FALSE, sep = "") {
    assert_that(is.character(x))
    assert_that(is.string(delim))
    s <- strsplit(x, delim)
	tfun <- function(y) {
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
unwanted_columns <- function(type) {
    type <- match.arg(tolower(type),c("general","layers","occurrence","occurrence_stored","occurrence_indexed","assertions"))
    switch(type,
           "general"=c("rawRank","rawRankString","rankId","rankID","left","right","idxType","highlight","linkIdentifier","isExcluded"),
             ## rawRank appears to be a duplicate of rank or rankString
           "layers"=c("pid","path","path_orig","path_1km","enabled","uid","licence_level","lookuptablepath","mdhrlv","mddatest","datalang","grid","shape","enabled","indb","spid","sid","sdesc","sname","defaultlayer","namesearch","intersect","layerbranch","analysis","addtomap"),
             ## datalang appears to be all "eng" "Eng" "enu" "" or NA (2x"enu" records appear to be in English and from DEH/DEWHA)
             ## grid is redundant: all env layers are grid==TRUE, all contextual layers are grid==NA
             ## ditto for shape: all contextual are TRUE, all grid are NA
             ## mddatest is an internal metadata testing date of some sort?
             ## enabled appears to be all TRUE
             ## spid is redundant with id
             ## no idea what sid,sname, or sdesc are, but don't look particularly useful in our context
           "occurrence_stored"=,
           "occurrence_indexed"=,
           "occurrence"=c("lft","rgt","rankId"),
             ## lft and rgt look the same as left and right in general fields
           c("")
           )
}

##----------------------------------------------------------------------------------------------

rename_variables <- function(varnames,type,verbose=ala_config()$verbose) {
    if (length(varnames)<1) {
        ## catch in case names from empty data frame were passed
        return(varnames)
    }
    assert_that(is.character(varnames))
    assert_that(is.string(type))
    type <- match.arg(tolower(type),c("general","layers","occurrence","occurrence_stored","occurrence_indexed","assertions","other")) ## use "other" to make no variable name substtutions, just enforce case/separator conventions
    
    ## change all to camelCase
    varnames <- tocamel(make.names(varnames))
    ## try to convert some all-lowercase names to camel, e.g. environmentalvaluemax minlatitude minlongitude
    for (kw in c("longitude","latitude","value","units")) {
        varnames <- str_replace_all(varnames,kw,paste(toupper(substring(kw,1,1)),substring(kw,2),sep=""))
    }
    ## some that only seem to appear at the ends of variable names, so be conservative with these replacements
    for (kw in c("min","max","path")) {
        varnames <- str_replace_all(varnames,paste(kw,"$",sep=""),paste(toupper(substr(kw,1,1)),substring(kw,2),sep=""))
    }        
    ## enforce first letter lowercase
    varnames <- paste(tolower(substr(varnames,1,1)),substring(varnames,2),sep="")
    ## some global re-naming by data type
    if (type=="general") {
        ## general names, from e.g. name searching
        varnames[varnames=="occCount"] <- "occurrenceCount"
        varnames[varnames=="classs"] <- "class"
        if (!any(varnames=="commonName")) {
            varnames[varnames=="vernacularName"] <- "commonName" ## taxinfo_download provides "vernacularName", others "commonName"
            varnames[varnames=="commonNameSingle"] <- "commonName" ## search_guids provides "commonNameSingle", others "commonName"
        }
        varnames <- str_replace_all(varnames,"conservationStatusInAustralia","conservationStatusAUS")
        varnames <- str_replace_all(varnames,"conservationStatusIn","conservationStatus")
        varnames <- str_replace_all(varnames,"scientificNameForAcceptedConcept","acceptedConceptName") ## taxinfo_download returns the former, but should be the latter for consistency elsewhere
        
        if (any(varnames=="rank") & any(varnames=="rankString")) {
            if (verbose) {
                warning("data contains both \"rank\" and \"rankString\" columns, not renaming \"rankString\"")
            }
        } else {
            varnames[varnames=="rankString"] <- "rank" ## returned as "rank" by some services and "rankString" by others
        }
        ## ditto for taxonRank
        if (any(varnames=="rank") & any(varnames=="taxonRank")) {
            if (verbose) {
                warning("data contains both \"rank\" and \"taxonRank\" columns, not renaming \"taxonRank\"")
            }
        } else {
            varnames[varnames=="taxonRank"] <- "rank" ## returned as "Taxon.Rank" (camelcased to "taxonRank") by taxinfo_download
        }
    } else if (type=="layers") {
        varnames[varnames=="desc"] <- "description"
    } else if (type %in% c("occurrence","occurrence_stored","occurrence_indexed")) {
        ## "scientificName" is actually scientificNameOriginal
        varnames[varnames=="scientificName"] <- "scientificNameOriginal"
        ## and "matchedScientificName" will get changed to "scientificName" below
        varnames[varnames=="recordID"] <- "id"
        varnames[varnames=="xVersion"] <- "version" ## is actually "_version_" in web service
        varnames <- str_replace_all(varnames,regex("axonconceptguid",ignore_case=TRUE),"axonConceptLsid")                
        varnames <- str_replace_all(varnames,"vernacularName","commonName")
        varnames <- str_replace_all(varnames,"taxonRank","rank")
        ## rawSomething to somethingOriginal
        varnames <- str_replace_all(varnames,"^raw(.*)$","\\1Original") ## first-letter lowercase will be lost here but gets fixed below
        ## dump "matched", "processed", and "parsed"
        varnames <- str_replace_all(varnames,regex("(matched|processed|parsed)",ignore_case=TRUE),"")
    } else if (type=="assertions") {
        a <- ala_fields("assertions",as_is=TRUE)
        ## want all assertion field names to match those in a$name
        ## but some may be camelCased versions of the description
        a$description <- rename_variables(a$description,type="other") ## use "other" here to avoid this renaming code block, just apply camelCasing etc
        varnames <- sapply(varnames,function(z){ifelse(z %in% a$name,z,ifelse(sum(z==a$description)==1,a$name[a$description==z],z))})
    }
    ## do this again, it may have been lost in the processing: enforce first letter lowercase
    varnames <- paste(tolower(substr(varnames,1,1)),substring(varnames,2),sep="")
    if (type %in% c("layers","occurrence","occurrence_stored","occurrence_indexed")) {
        ## but some acronyms in layer names should remain all-uppercase
        ## currently this list is: c("iBRA","iMCRA","aCTTAMS","gER","nZ","nSW","lGA","nRM","rAMSAR","nDVI","nPP","aSRI","gEOMACS")
        ## but since these all occur at the start of variable names, we can catch them with a regular expression and not need to hard-code a list
        idx <- str_detect(varnames,"^[a-z][A-Z]")
        temp <- varnames[idx]
        varnames[idx] <- paste(toupper(substr(temp,1,1)),substring(temp,2),sep="")
        ## "seaWIFS" to "SeaWIFS"
        varnames <- str_replace_all(varnames,"seaWIFS","SeaWIFS")
    }        

    if (type=="assertions") { ###hardcoded assertion variable name changes
        ## these assertions come back from the ALA service with the wrong names
        if ("coordinatesAreOutOfRangeForSpecies" %in% varnames) varnames[varnames=="coordinatesAreOutOfRangeForSpecies"] <- "coordinatesOutOfRange"
        if ("collectionDateMissing" %in% varnames) varnames[varnames=="collectionDateMissing"] <- "missingCollectionDate"
        if ("coordinateUncertaintyNotSpecified" %in% varnames) varnames[varnames=="coordinateUncertaintyNotSpecified"] <- "uncertaintyNotSpecified"
    }	
    ##return the varnames
    varnames
}

## construct url path, taking care to remove multiple forward slashes, leading slash
clean_path <- function(...,sep="/") {
    path1 <- sapply(list(...),FUN=function(z)paste(z,sep=sep,collapse=sep)) ## collapse individual arguments
    ## workaround to avoid replacing "http://" with "http:/", since this is now used in GUID strings (July 2016)
    path <- paste(path1,sep=sep,collapse=sep) ## paste parts together
    path <- gsub("http://","http:@@",path,fixed=TRUE)
    path <- gsub(paste0("[",sep,"]+"),sep,path) ## remove multiple slashes
    path <- gsub("http:@@","http://",path,fixed=TRUE)
    sub(paste0("^",sep),"",path) ## remove leading slash
}

## convenience function for building urls
## pass path in one of several ways
##  as single string: build_url_from_parts(base_url,"path/to/thing")
##  as a character vector or list: build_url_from_parts(base_url,c("path","to","thing"))
##  or a combination
build_url_from_parts <- function(base_url,path=NULL,query=list()) {
    this_url <- parse_url(base_url)
    this_url$path <- clean_path(this_url$path,path)
    if (length(query)>0) {
        this_url$query <- query
    }
    build_url(this_url)
}    


## wrapper around read.csv but suppressing "incomplete final line" warning
read_csv_quietly <- function(...) {
    read_warnings <- NULL
    w_handler <- function(w) { if (!grepl("incomplete final line",as.character(w),ignore.case=TRUE))
                                read_warnings <<- c(read_warnings,list(w))
                            invokeRestart("muffleWarning")
                        }
    out <- withCallingHandlers({ read.csv(...) }, warning=w_handler)
    ## now throw any warnings that got collected, because they weren't about a final missing line break
    for (w in read_warnings) warning(w)
    out
}

replace_nonbreaking_spaces <- function(s)
    gsub("\ua0"," ",s)
