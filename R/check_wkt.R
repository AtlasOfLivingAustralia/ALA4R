# simple function to check that a given WKT string is valid
check_wkt=function(wkt) {
    wkt_OK=FALSE
    try({ readWKT(wkt);wkt_OK=TRUE },silent=TRUE)
    wkt_OK
}
