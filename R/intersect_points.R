#' Intersect environmental or contextual layers at a given a set of points (coordinates)
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @references \url{http://spatial.ala.org.au/layers/}
#' 
#' @param points vector of lat & lons or 2 column data.frame or matrix of lat,lons. NOTE: the number of locations must be less than 1000.
#' @param fids text: ids of layers to be intersected. list of possible layers is available from ala_fields().
#' @param SPdataframe boolean value defining if the output should be returned as a SpatialPointsDataFrame of the sp package.
#' @param verbose boolean value defining how much progress information to display; default is set by ala_config().
#' @return A SpatialPointsDataFrame containing the intersecting data information. Missing data or incorrectly identified field id values will result in NA data
#'
#'TODO: Pity that "fields" rather than "layers" are used. Confusing.
#'
#' @examples
#' \dontrun{
#' #single point with multiple fields
#' fields = c('cl22','cl23','el773')
#' pnts = c(-23.1,149.1)
#' intersect_points(pnts,fields)
#'
#' #Web Service: http://spatial.ala.org.au/ws/intersect/cl22,cl23/-23.1/149.1  
#' 
#' #multiple points with multiple fields
#' fields = c('cl22','cl23','el773')
#' pnts = data.frame(lat=seq(-29,-19,0.02),lon=132.769)
#' intersect_points(pnts,fields)
#' 
#' }
#' @export

intersect_points = function(pnts,fids,SPdata.frame=FALSE,verbose=ala_config()$verbose) {
	base_url=ala_config()$base_url_spatial #get the base url
	bulk = FALSE #set the default to not bulk

	###check and format the points
	if (class(pnts) %in% c('data.frame','matrix')) { #convert to a vector if a data.frame or matrix and setup the string for the url
		if (dim(pnts)[2] !=2) stop('data.frame or matrix of points must have 2 columns ordered lat,lon') #check the dimensions
		if (nrow(pnts)==1) { #this is for a single coordinate pair
			pnts_str = paste(pnts[1,],collapse='/',sep='') #setup the points str for the url
		} else { #this is for the bulk intersect process where there is more than 1 coordinate pairs
			if (nrow(pnts)>1001) stop('number of locations checked must be less than 1000') #ensure maximum limit is not breached
			pnts_str = paste(paste(pnts[,1],pnts[,2],sep=','),collapse=',',sep='') #setup the points str for the url
			bulk = TRUE #this is a bulk set of points
		}
	} else { #format the vector as a string for the url
		if (length(pnts) %% 2 == 1) stop('vector of points must be paired locations... the length of this vector must be even') #check that the length of this is even 
		if (length(pnts) == 2) { #this is for the single coordinate pair
			pnts_str = paste(pnts,collapse='/',sep='') #setup the points str for the url
		} else {  #this is for the bulk intersect process where there is more than 1 coordinate pairs
			if (nrow(pnts)>2001) stop('number of locations checked must be less than 1000') #ensure maximum limit is not breached
			pnts_str = paste(pnts,collapse=',',sep='') #setup the points str for the url
			bulk = TRUE #this is a bulk set of points
		}
	}
	###format the fields string
	if (length(fids)>1) {
		fids_str = paste(fids,collapse=',',sep='')
	} else {
		fids_str = fids
	}

	###download the data
	if (bulk) { #get the results if it is a bulk request
		url_str = paste(base_url,'intersect/batch?fids=',fids_str,'&points=',pnts_str,sep='') #define the url string
		this_cache_file=ala_cache_filename(url_str) ## the file that will ultimately hold the results (even if we are not caching, it still gets saved to file)
		if ((ala_config()$caching %in% c("off","refresh")) || (! file.exists(this_cache_file))) {
			## fetch the data from the server
			## we use cached_get operations here even though we're not caching, just because it keeps the user-agent etc string consistent
			#status_url=cached_get(url_str,type="json",caching="off")$statusUrl #submit the url and get the url of the status
			status_url=jsonlite::fromJSON(paste(readLines(url_str,warn=FALSE),collapse = ""))$statusUrl ## using GET (in cached_get) for this can give 414 errors ("url too long") which do not seem to happen with readLines, so use that until we figure out why
			data_url=cached_get(status_url,type="json",caching="off") #get the data url
			while (data_url$status != 'finished') { #keep checking the status until finished
				Sys.sleep(5)
				data_url=cached_get(status_url,type="json",caching="off") #get the data url
			}
			download_to_file(data_url$downloadUrl,outfile=this_cache_file)
		} else {
			## we are using the existing cached file
			if (verbose) { cat(sprintf("  ALA4R: using cached file %s\n",this_cache_file)) }
		}
		out = read.csv(unz(this_cache_file,'sample.csv'),as.is=TRUE) #read in the csv data from the zip file
		checks = NULL; for (ii in colnames(out)) { if (all(is.na(out[,ii]))) checks = c(checks,ii) } #identify bad field IDs
		if (!is.null(checks)) warning(paste(paste(checks,collapse=', '),'are invalid field ids')) #warn user of bad field ids
	} else { #get results if just a single location
		url_str = paste(base_url,'intersect/',fids_str,'/',pnts_str,sep='') #define the url string
		out = cached_get(url_str,type="json") #get the data
		if (length(out)==0) stop('all field ids provided were invalid') #nothing returned if no valid IDs provided
		tt = t(out$value); colnames(tt) = out$field 
		out = data.frame(latitude=pnts[1],longitude=pnts[2],tt) # define the output the same as the bulk output
		#if NOT using jsonlite
		#    out = do.call('rbind.fill',lapply(out,as.data.frame,stringsAsFactors=FALSE)) #define the output
		#}
		checks = setdiff(fids,colnames(out)[-c(1,2)]); if (length(checks)>0) warning(paste(paste(checks,collapse=', '),'are invalid field ids')) #warn user of bad field ids
	}
	###deal with SpatialPointsDataFrame
	if (SPdata.frame) { #if output is requested as a SpatialPointsDataFrame
		if (is.element('sp', installed.packages()[,1])) { #if sp package is available
			require(sp) #load the library		
			## coerce to SpatialPointsDataFrame class
			if (nrow(out)>0) {
				out=SpatialPointsDataFrame(coords=out[,c("longitude","latitude")],proj4string=CRS("+proj=longlat +ellps=WGS84"),data=out)
			}
		} else {
			warning('sp package needs to be installed; data output is output as a simple dataframe')
		}
	}
	###return the output
    return(out)
}
