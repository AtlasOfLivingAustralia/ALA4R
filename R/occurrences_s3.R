#' Summarize, filter and subset occurrence data
#' 
#' Set of S3 methods to summarize, filter and get unique occurrence data retrieved using \code{occurrences}. 
#' This uses information based on selections of assertions (quality assurance issues ALA has identified), spatial and temporal data.
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' 
#' @param object list: an 'occurrence' object that has been downloaded using \code{occurrences}
#' @param x list: an 'occurrence' object that has been downloaded using \code{occurrences}
#' @param spatial numeric: specifies a rounding value in decimal degrees used to to create a unique subset of the data. Value of 0 means no rounding and use values as is. Values <0 mean ignore spatial unique parameter
#' @param incomparables logical/numeric: currently ignored, but needed for S3 method consistency
#' @param \dots not currently used
#'
#' @details
#' \code{unique} will give the min value for all columns that are not used in the aggregation.
#' 
#' @examples
#' #download some observations
#' x=occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)
#' 
#' #summarize the occurrences
#' summary(x)
#' 
#' #keep spatially unique data at 0.01 degrees (latitude and longitude)
#' tt = unique(x,spatial=0.01)
#' summary(tt)
#' 
#' @name occurrences_s3
NULL

#' @rdname occurrences_s3
#' @S3method summary occurrences
"summary.occurrences" <- function(object, ...) {
	cat('number of names:',length(unique(object$data$Scientific.Name)),'\n')
	cat('number of taxonomically corrected names:',length(unique(object$data$Matched.Scientific.Name)),'\n')
	cat('number of observation records:',nrow(object$data),'\n')
	ass = check_assertions(object) #need to get existing assertions in occur dataset
	if (nrow(ass)>0) {
		cat('assertions checked:',nrow(ass),'\n')
		for (ii in 1:nrow(ass)) {
			rwi = length(which(as.logical(object$data[,ass$occur.colnames[ii]])==TRUE)) #count the number of records with issues
			if (rwi>0) cat('\t',ass$occur.colnames[ii],': ',rwi,' records ',ifelse(as.logical(ass$fatal[ii]),'-- considered fatal',''),sep='','\n')
		}
	} else { cat('no asserting issues\n') }
	invisible(object)
}

#' @rdname occurrences_s3
#' @S3method unique occurrences
"unique.occurrences" <- function(x, incomparables=FALSE, spatial=0, ...) {
    assert_that(is.numeric(spatial)) #ensure unique.spatial is numeric
    if (spatial<0) {
        cat('ignoring spatial \n')
    } else {
        if (spatial>0) { #round the data to the spatial accuracy of interest
            x$data$Latitude...processed = round(x$data$Latitude...processed / spatial) * spatial
            x$data$Longitude...processed = round(x$data$Longitude...processed / spatial) * spatial
        }
        tt = x$data[,c("Species...matched","Latitude...processed","Longitude...processed")] #keep only spatial and species data
        tt = aggregate(x$data$Species...matched,by=list(tt$Species...matched,tt$Latitude...processed,tt$Longitude...processed),length) #get the number of 'unique' spatial data
        for (ii in 1:nrow(tt)) { #cycle through each of the unique values
            if (tt$x[ii] > 1) {
                roi = which(x$data$Species...matched==tt[ii,1] & x$data$Latitude...processed==tt[ii,2] & x$data$Longitude...processed==tt[ii,3]) #get the rows of interest
                for (jj in colnames(x$data)) x$data[roi,jj] = min(x$data[roi,jj]) #replace all data with the minimum
            }			
        }
        x$data = unique(x$data) #keep the unique data
    } 
    x
}
