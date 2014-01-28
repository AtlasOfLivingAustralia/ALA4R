#' Intersect a layer(s) at a given set of coordinates
#' 
#' Intersect environmental or contextual layers given a set of coordinates
#' 
#' @param pnts vector of lat & lons or 2 column data.frame or matrix of lat,lons. NOTE: the number of locations must be less than 1000.
#' @param field_ids text: id of field for which to look up information. list of possible fields available from fields().
#' @return A data frame containing the intersecting data information. Missing data or incorrectly identified field id values will result in NA data
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}, Ben Raymond \email{ben@@theraymonds.org}
#' @references \url{http://spatial.ala.org.au/ws/}
#' @examples
#' 
#' #single point with multiple fields
#' fields = c('cl22','cl23')
#' pnts = c(-29,132.999)
#' intersect(pnts,fields)
#' 
#' #multiple points with multiple fields
#' fields = c('cl22','cl23')
#' pnts = data.frame(lat=seq(-29,-19,0.02),lon=132.769)
#' intersect(pnts,fields)
#'
#' @export

intersect = function(pnts,fid) {
	base_url=ala_config()$base_url_spatial #get the base url
	bulk = FALSE #set the default to not bulk

	###check and format the points
	if (class(pnts) %in% c('data.frame','matrix')) { #convert to a vector if a data.frame or matrix
		if (dim(pnts)[2] !=2) stop('data.frame or matrix of points must have 2 columns ordered lat,lon') #check the dimensions
		if (nrow(pnts)==1) {
			pnts_str = paste(pnts[1,],collapse='/',sep='')
		} else {
			if (nrow(pnts)>1001) stop('number of locations checked must be less than 1000') #ensure maximum limit is not breached
			pnts_str = paste(paste(pnts[,1],pnts[,2],sep=','),collapse=',',sep='')
			bulk = TRUE #this is a bulk set of points
		}
	} else {
		if (length(pnts) %% 2 == 1) stop('vector of points must be paired locations... the length of this vector must be even') #check that the length of this is even 
		if (length(pnts) == 2) {
			pnts_str = paste(pnts,collapse='/',sep='')
		} else {
			if (nrow(pnts)>2001) stop('number of locations checked must be less than 1000') #ensure maximum limit is not breached
			pnts_str = paste(pnts,collapse=',',sep='')
			bulk = TRUE #this is a bulk set of points
		}
	}

	###format the fields string
	if (length(fields)>1) {
		fields_str = paste(fields,collapse=',',sep='')
	} else {
		fields_str = fields
	}

	###download the data
	if (bulk) { #get the results if it is a bulk request
		url_str = paste(base_url,'intersect/batch?fids=',fields_str,'&points=',pnts_str,sep='') #define the url string
		status_url = fromJSON(file=url_str)$statusUrl #submit the url and get the url of the status
		data_url = fromJSON(file=status_url) #get the data url
		while (data_url$status != 'finished') { #keep checking the status until finished
			Sys.sleep(5)
			data_url = fromJSON(file=status_url) #get the data url
		}
		###not using caching because each query to this system will provide unique id & unique URL
		tmpfile = tempfile() #define a temporary file
		download.file(data_url$downloadUrl,tmpfile,mode='wb')
		out = read.csv(unz(tmpfile,'sample.csv'),as.is=TRUE) #read in the csv data from the zip file
		out
	} else { #get results if just a single location
		url_str = paste(base_url,'intersect/',fields_str,'/',pnts_str,sep='') #define the url string
		out = cached_get(url_str,type="json") #get the data
		as.data.frame(do.call('rbind',out)) #define the output
	}
	
}

