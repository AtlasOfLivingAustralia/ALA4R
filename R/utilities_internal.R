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


rename_variables=function(varnames,type) {
    assert_that(is.character(varnames))
    assert_that(is.string(type))
    type=match.arg(tolower(type),c("general","layers","occurrence","assertions"))
    if (TRUE) {
        ## for now, just return the names as-is, but enforce validity as variable names
        make.names(varnames)
     } else {
        ## proposal to move to camelCase
        tocamel(make.names(varnames))
    }
}
