# simple function to check that a given WKT string is valid
check_wkt=function(wkt,silent=TRUE) {
    assert_that(is.string(wkt))
    wkt_OK=FALSE
    ## readWKT depends on rgeos, which we want to avoid
    ##try({ readWKT(wkt);wkt_OK=TRUE },silent=TRUE)
    
    ## instead we do some lightweight WKT checking here, and rely on the error message from the server to identify more subtle errors in the WKT string
    if (nchar(wkt)<1) {
        ## empty string: treat as invalid wkt
        return(FALSE)
    }
    ## we only check POLYGONs here
    if (grepl("^polygon",tolower(str_trim(wkt)))) {
        is_valid_wkt_polygon(wkt)
    } else {
        if (!silent) {
            warning("ALA4R cannot check this type of WKT string")
        }
        NA # return NA, i.e. we don't know if it's valid
    }
}



is_valid_wkt_polygon=function(wkt) {
    ## expect to start with something like "POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))"
    points_group=str_match(tolower(str_trim(wkt)),"^polygon\\s*\\(\\s*\\((.+)\\)\\s*\\)$")
    if (!is.na(points_group[1,2])) {
        points_group=points_group[1,2]
        ## should now have e.g. "140 -37,151 -37,151 -26,140 -26,140 -37"
        points_list=str_split(points_group,",")
        points_list=points_list[[1]]
        ## "140 -37" "151 -37" "151 -26" "140 -26" "140 -37"
        grepl("^[[:digit:]\\.\\-]+\\s+[[:digit:]\\.\\-]+$",points_list) && identical(points_list[1],points_list[length(points_list)])
    } else {
        FALSE
    }
}    

