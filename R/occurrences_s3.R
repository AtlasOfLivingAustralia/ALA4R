#' Summarize, filter and subset occurrence data
#' 
#' Set of S3 methods to summarize, filter and get unique occurrence data retrieved using \code{\link{occurrences}}.
#' This uses information based on selections of assertions (quality assurance issues ALA has identified), spatial and temporal data.
#' 
#' @references \url{http://api.ala.org.au/}
#' @references \url{http://stat.ethz.ch/R-manual/R-devel/library/methods/html/Methods.html}
#' 
#' @param object list: an 'occurrence' object that has been downloaded using \code{\link{occurrences}}
#' @param x list: an 'occurrence' object that has been downloaded using \code{\link{occurrences}}
#' @param incomparables logical/numeric: currently ignored, but needed for S3 method consistency
#' @param spatial numeric: specifies a rounding value in decimal degrees used to to create a unique subset of the data. Value of 0 means no rounding and use values as is. Values <0 mean ignore spatial unique parameter
#' @param temporal character: specifies the temporal unit for which to keep unique records; this can be by "year", "month", "yearmonth" or "full" date. NULL means ignore temporal unique parameter
#' @param na.rm logical: keep (FALSE) or remove (TRUE) missing spatial or temporal data
#' @param remove.fatal logical: remove flagged assertion issues that are considered "fatal"; see \code{\link{check_assertions}} 
#' @param exclude.spatial character vector: defining flagged spatial assertion issues to be removed. Values can include 'warnings','error','missing','none'; see \code{\link{check_assertions}}
#' @param exclude.temporal character vector: defining flagged temporal assertion issues to be removed. Values can include 'warnings','error','missing','none'; see \code{\link{check_assertions}}
#' @param exclude.taxonomic character vector: defining flagged taxonomic assertion issues to be removed. Values can include 'warnings','error','missing','none'; see \code{\link{check_assertions}}
#' @param max.spatial.uncertainty numeric: number defining the maximum spatial uncertainty (in meters) one is willing to accept. 
#' @param keep.missing.spatial.uncertainty logical: keep (FALSE) or remove (TRUE) information missing spatial uncertainty data.
#' @param \dots not currently used
#'
#' @details
#' \code{unique} will give the min value for all columns that are not used in the aggregation.
#' 
#' @examples
#' \dontrun{
#' #download some observations
#' x <- occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)
#' 
#' #summarize the occurrences
#' summary(x)
#' 
#' #keep spatially unique data at 0.01 degrees (latitude and longitude)
#' tt <- unique(x,spatial=0.01)
#' summary(tt)
#'
#' #keep spatially unique data that is also unique year/month for the collection date
#' tt <- unique(x,spatial=0,temporal='yearmonth')
#' summary(tt)
#'
#' #keep only information for which fatal or "error" assertions do not exist
#' tt <- subset(x)
#' summary(tt)
#' }
#' @name occurrences_s3
NULL

#' @rdname occurrences_s3
#' @method summary occurrences
#' @export
"summary.occurrences" <- function(object, ...) {
    ## names are a little problematic at the moment: sometimes scientificName doesn't come back (being resolved in web service I hope)
    if ("scientificName" %in% names(object$data)) {
        if ("scientificNameOriginal" %in% names(object$data))
            cat('number of original names:',length(unique(object$data$scientificNameOriginal)),'\n')
        cat('number of taxonomically corrected names:',length(unique(object$data$scientificName)),'\n')
    } else if ("taxonName" %in% names(object$data)) {
        if ("taxonNameOriginal" %in% names(object$data))
            cat('number of original names:',length(unique(object$data$taxonNameOriginal)),'\n')
        cat('number of taxonomically corrected names:',length(unique(object$data$taxonName)),'\n')
    }
    cat('number of observation records:',nrow(object$data),'\n')
    ass <- suppressWarnings(check_assertions(object)) #need to get existing assertions in occur dataset
    if (is.null(ass)) {
        cat('no assertion issues\n')
    } else {
        cat('number of assertions listed:',nrow(ass),' -- ones with flagged issues are listed below\n')
        for (ii in 1:nrow(ass)) {
            rwi <- length(which(as.logical(object$data[,ass$occurColnames[ii]])==TRUE)) #count the number of records with issues
            if (rwi>0) cat('\t',ass$occurColnames[ii],': ',rwi,' records ',ifelse(as.logical(ass$fatal[ii]),'-- considered fatal',''),sep='','\n')
        }
    } 
    invisible(object)
}

#' @rdname occurrences_s3
#' @method unique occurrences
#' @export
"unique.occurrences" <- function(x, incomparables=FALSE, spatial=0, temporal=NULL, na.rm=FALSE, ...) {
    ## helper function to make sure names are present
    check_names_present <- function(nms) {
        if (!all(nms %in% names(x$data))) stop(sprintf("expecting columns '%s' in occurrences data. %s",paste(setdiff(nms,names(x)),collapse="','"),getOption("ALA4R_server_config")$notify))
        invisible(TRUE)
    }
    assert_that(is.numeric(spatial)) #ensure unique.spatial is numeric
    if (!is.null(temporal)) {
        if (!temporal %in% c('year','month', 'yearmonth','full')) stop('temporal value must be NULL, "year", "month", "yearmonth" or "full"')
    }
    check_names_present("scientificName")
    cois <- list(scientificName=x$data$scientificName) #start defining the columns of interest to do the "unique" by
    if (spatial>=0) {
        check_names_present(c("longitude","latitude"))
        if (spatial>0) { #round the data to the spatial accuracy of interest
            x$data$latitude <- round(x$data$latitude / spatial) * spatial
            x$data$longitude <- round(x$data$longitude / spatial) * spatial
        }
        cois$latitude <- x$data$latitude; cois$longitude <- x$data$longitude #append the latitude and longitude
    }
    if (!is.null(temporal)) {
        if (temporal=='full') {
            check_names_present("eventDate")
            cois$eventDate <- x$data$eventDate #add the full date to cois
        } else {
            check_names_present(c("month","year"))
            if (length(grep('month',temporal))>0) cois$month <- x$data$month
            if (length(grep('year',temporal))>0) cois$year <- x$data$year
        }
    }
    cat('extracting unique data using columns: ', paste(names(cois),collapse=','),'\n')
	x$data <- aggregate(x$data,by=cois,min)[,-c(1:length(names(cois)))] #get 'unique' spatial/temporal data
    if (na.rm) {
        rois <- which(is.na(x$data[,names(cois)]),arr.ind=TRUE)[,1]
        if ('eventDate' %in% names(cois)) rois <- c(rois,which(x$data$eventDate==""))
        if (length(rois)>0) x$data <- x$data[-(unique(rois)),] #remove the missing data rows
    }
    x
}

#' @rdname occurrences_s3
#' @method subset occurrences
#' @export
"subset.occurrences" <- function(x, remove.fatal=TRUE, exclude.spatial='error', exclude.temporal='error', 
	exclude.taxonomic='error', max.spatial.uncertainty, keep.missing.spatial.uncertainty=TRUE, ...) 
{
    assert_that(is.character(exclude.spatial))
    assert_that(is.character(exclude.temporal))
    assert_that(is.character(exclude.taxonomic)) #check assertions are character vectors
    if(!all(c(exclude.spatial,exclude.temporal,exclude.temporal) %in% c('warnings','error','missing','none'))) {
        stop("exclude spatial, temporal and taxonomic must be a vector containing words of 'warnings','error','missing' or 'none'")
    }
    assert_that(is.flag(remove.fatal)) #ensure fatal is logical flag
    assert_that(is.flag(keep.missing.spatial.uncertainty))
    
    ass <- check_assertions(x) #need to get existing assertions in occur dataset
    roi <- NULL #define an object outlining rows to remove
    
    if (is.null(ass)) {
        warning('no assertions in occurrence data')
    } else {
        for (ii in 1:nrow(ass)) {
            if (ass$fatal[ii]==TRUE) {
                if (remove.fatal) { #remove the fatal data
                    roi <- c(roi, which(x$data[,ass$occurColnames[ii]] == TRUE)); next
                } 
            }
            if (ass$code[ii] < 10000) { #remove data with spatial issues
                if (length(exclude.spatial)>0) {
                    if (ass$category[ii] %in% exclude.spatial) {
                        roi <- c(roi, which(x$data[,ass$occurColnames[ii]] == TRUE)); next
                    }
                }
            } else if (ass$code[ii] >= 10000 & ass$code[ii] < 20000) { #remove data with taxonomic issues
                if (length(exclude.taxonomic)>0 ) {
                    if (ass$category[ii] %in% exclude.taxonomic) {
                        roi <- c(roi, which(x$data[,ass$occurColnames[ii]] == TRUE)); next
                    }
                }		
            } else if (ass$code[ii] >= 30000) { #remove data with temporal issues
                if (length(exclude.temporal)>0 ) {
                    if (ass$category[ii] %in% exclude.temporal) {
                        roi <- c(roi, which(x$data[,ass$occurColnames[ii]] == TRUE)); next
                    }
                }		
            }
        }
    }
    if (!missing(max.spatial.uncertainty)) {
        assert_that(is.numeric(max.spatial.uncertainty))
        if (! "coordinateUncertaintyInMetres" %in% names(x$data)) {
            warning("column \"coordinateUncertaintyInMetres\" is not present in this occurrences object: ignoring max.spatial.uncertainty parameter")
        } else {
            if (keep.missing.spatial.uncertainty==FALSE) {
                roi <- c(roi,which(is.na(x$data$coordinateUncertaintyInMetres)))
            }
            roi <- c(roi,which(x$data$coordinateUncertaintyInMetres<=max.spatial.uncertainty))
        }
    }
    roi <- unique(roi) #remove duplicates
    if (length(roi)>0) x$data <- x$data[-roi,] #remove the data
    x
}

