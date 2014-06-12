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
