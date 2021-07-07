.onAttach <- function(libname, pkgname) {
  msg <- "`ALA4R` is deprecated and will be removed from CRAN at the end of 2021. The package to replace `ALA4R``,
`galah`, is now available (https://CRAN.R-project.org/package=galah). For an introduction to `galah`,
visit the GitHub page (https://github.com/AtlasOfLivingAustralia/galah)."
  packageStartupMessage(msg)
}
