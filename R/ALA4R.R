#' \pkg{ALA4R}
#'
#' This project enables the R community to access data and tools hosted by the 
#' Atlas of Living Australia (ALA: http://www.ala.org.au). The goal of the project is 
#' to enable basic species and related information to be queried and output in 
#' standard formats for R. ALA4R is based around use of the extensive web services provided 
#' by the Atlas; see References below for documentation of these web services.
#' 
#' @name ALA4R
#' @author Atlas of Living Australia \email{support@@ala.org.au}, Ben Raymond, Jeremy VanDerWal, Lee Belbin
#' @docType package
#' @references \url{http://api.ala.org.au/}
#' @import httr plyr digest RCurl jsonlite assertthat sp
#' @importFrom stringr regex str_c str_detect str_extract str_locate str_match str_match_all
#' @importFrom stringr str_replace str_replace_all str_split str_trim
#' @importFrom wellknown lint
#' @importFrom grDevices dev.off pdf rainbow
#' @importFrom graphics image legend points title
#' @importFrom stats aggregate na.omit
#' @importFrom utils data packageVersion read.csv read.table str unzip URLencode 
NULL
