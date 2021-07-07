#' \pkg{ALA4R}
#' ALA4R
#' 
#' `ALA4R` is deprecated and will be removed from CRAN at the end of 2021.  The package to replace `ALA4R``, 
#' `galah`, is now available on CRAN \url{https://CRAN.R-project.org/package=galah}.
#' `galah` provides an improved interface to ALA data, while providing the same
#' core functionality as ALA4R. For an introduction to `galah`, visit the
#' GitHub page \url{https://github.com/AtlasOfLivingAustralia/galah}.
#' 
#' This project enables the R community to access data and tools hosted by the
#' Atlas of Living Australia. The goal of the project is
#' to enable basic species and related information to be queried and output in
#' standard formats for R. ALA4R is based around the extensive web services
#' provided by the Atlas; see the API link below.
#' 
#' @name ALA4R
#' @docType package
#' @references \url{https://api.ala.org.au/}
#' @import assertthat httr sp
#' @importFrom digest digest
#' @importFrom grDevices dev.off pdf rainbow
#' @importFrom graphics image legend points title
#' @importFrom jsonlite fromJSON
#' @importFrom stats aggregate na.omit
#' @importFrom stringr regex str_c str_detect str_extract str_locate 
#' str_match str_match_all
#' @importFrom stringr str_replace str_replace_all str_split str_trim
#' @importFrom utils data packageVersion read.csv read.table str unzip URLencode
#' @importFrom wellknown lint
NULL
