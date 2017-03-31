.onAttach <- function(libname, pkgname) {
    packageStartupMessage("*NOTE, April 2017* --- some ALA4R functions may not work correctly due to back-end server changes. This includes the occurrences() function, and full-text searching [particularly specieslist(taxon=...)]. The issue is being investigated.")
}
