.onAttach <- function(libname, pkgname) {
  msg <- "`ALA4R` is deprecated; we suggest you use `galah`instead (https://CRAN.R-project.org/package=galah). 
    For an introduction to `galah`, visit the GitHub page (https://github.com/AtlasOfLivingAustralia/galah)."
  packageStartupMessage(msg)
}
