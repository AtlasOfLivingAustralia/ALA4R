#' Intersect environmental or contextual layers at a given a set of points (coordinates)
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @references \url{http://spatial.ala.org.au/layers/}
#'
#' This function allows the user to sample environmental/contextual layers at arbitrary locations. It complements 
#' the \code{\link{occurrences}} function, which allows values of the same set of layers to be downloaded at 
#' species occurrence locations. NOTE: large requests may be slow. A single point (even with a large number of layers) will generally be OK; a large number of points may be very slow. Be warned.
#' @param pnts numeric: vector of latitude/longitude pairs, or a 2 column data.frame or matrix of lat,lons. NOTE: the number of locations must be less than 100000.
#' @param layers string vector: ids of layers to be intersected. The list of possible layers is available from \code{ala_fields("layers")}. Names can be passed as full layer names (e.g. "Radiation - lowest period (Bio22)") rather than id ("el871"). Note: if more than one location has been provided in \code{pnts}, the number of layers must be less than 700.
#' @param SPdata.frame logical: should the output should be returned as a SpatialPointsDataFrame of the sp package or simply as a data.frame?
#' @param use_layer_names logical: if TRUE, layer names will be used as column names in the returned data frame (e.g. "radiationLowestPeriodBio22"). Otherwise, layer id value will be used for column names (e.g. "el871")
#' @param verbose logical: show additional progress information? [default is set by \code{\link{ala_config}}]
#' @return A SpatialPointsDataFrame containing the intersecting data information. Missing data or incorrectly identified layer id values will result in NA data
#' @seealso \code{\link{ala_config}}
#' @examples
#' #single point with multiple layers
#' layers = c('cl22','cl23','el773')
#' pnts = c(-23.1,149.1)
#' intersect_points(pnts,layers)
#' # equivalent direct web service call: http://spatial.ala.org.au/ws/intersect/cl22,cl23,el773/-23.1/149.1  
#' \dontrun{
#' ## multiple points as a grid sampling multiple layers
#' ## not run here because it's pretty slow
#' layers = c('cl22','cl23','el773')
#' pnts = data.frame(expand.grid(lat=seq(-29,-19,2.0),lon=seq(130.0,140.0,2.0)))
#' intersect_points(pnts,layers)
#' }

## Previous limits of 1000 points and 299 layers have been increased here to reflect the increase on the service end. The batch version uses POST now to avoid 414 (URL too long) errors. Non-batch version does not seem to suffer 414s, even with 300 layers

#' @export
intersect_points = function(pnts,layers,SPdata.frame=FALSE,use_layer_names=TRUE,verbose=ala_config()$verbose) {
    ## input parameter checking
    assert_that(is.numeric(pnts) || all(apply(pnts,2,is.numeric)))
    assert_that(is.character(layers))
    assert_that(is.flag(SPdata.frame))
    assert_that(is.flag(verbose))

    num_points_limit=100000 # was previously 1000
    num_layers_limit=700 # was previously 300
    use_post=TRUE # use POST for batch operations, not GET?
    
    base_url=ala_config()$base_url_spatial #get the base url
    bulk = FALSE #set the default to not bulk

    ##check and format the points
    if (class(pnts) %in% c('data.frame','matrix')) { #convert to a vector if a data.frame or matrix and setup the string for the url
        if (dim(pnts)[2] !=2) stop('data.frame or matrix of points must have 2 columns ordered lat,lon') #check the dimensions
        if (nrow(pnts)==1) { #this is for a single coordinate pair
            pnts_str = paste(pnts[1,],collapse='/',sep='') #setup the points str for the url
        } else { #this is for the bulk intersect process where there is more than 1 coordinate pairs
            if (nrow(pnts)>(num_points_limit+1)) stop('number of locations checked must be less than ',num_points_limit) #ensure maximum limit is not breached
            pnts_str = paste(paste(pnts[,1],pnts[,2],sep=','),collapse=',',sep='') #setup the points str for the url
            bulk = TRUE #this is a bulk set of points
        }
    } else { #format the vector as a string for the url
        if (length(pnts) %% 2 == 1) stop('vector of points must be paired locations... the length of this vector must be even') #check that the length of this is even 
        if (length(pnts) == 2) { #this is for the single coordinate pair
            pnts_str = paste(pnts,collapse='/',sep='') #setup the points str for the url
        } else {  #this is for the bulk intersect process where there is more than 1 coordinate pairs
            if (length(pnts)>(num_points_limit*2+1)) stop('number of locations checked must be less than ',num_points_limit) #ensure maximum limit is not breached
            pnts_str = paste(pnts,collapse=',',sep='') #setup the points str for the url
            bulk = TRUE #this is a bulk set of points
        }
    }
    ##format the layers string
    layers=fields_name_to_id(fields=layers,fields_type="layers") ## replace long names with ids
    if (bulk) { if (length(layers)>(num_layers_limit-1)) stop('the number of layers must be <',num_layers_limit,' if intersecting more than a single location') } #ensure no more than 300 layers when bulk
    if (length(layers)>1) {
        layers_str = paste(layers,collapse=',',sep='')
    } else {
        layers_str = layers
    }
    
    ##download the data
    if (bulk) { #get the results if it is a bulk request
        if (use_post) {
            url_str=paste(base_url,"intersect/batch",sep="")
        } else {
            url_str = paste(base_url,'intersect/batch?fids=',layers_str,'&points=',pnts_str,sep='') #define the url string
            url_str=URLencode(url_str) ## should not be needed, but do it anyway
        }
        this_cache_file=ala_cache_filename(url_str) ## the file that will ultimately hold the results (even if we are not caching, it still gets saved to file)
        if ((ala_config()$caching %in% c("off","refresh")) || (! file.exists(this_cache_file))) {
            ## fetch the data from the server
            if (use_post) {
                status_url=jsonlite::fromJSON(cached_post(url_str,body=paste('fids=',layers_str,'&points=',pnts_str,sep=''),type="text"))$statusUrl
            } else {
                ## we use cached_get operations here even though we're not caching, just because it keeps the user-agent etc string consistent
                status_url=jsonlite::fromJSON(paste(readLines(url_str,warn=FALSE),collapse = ""))$statusUrl ## using GET (in cached_get) for this can give 414 errors ("url too long") which do not seem to happen with readLines, so use that until we figure out why
            }
            data_url=cached_get(status_url,type="json",caching="off") #get the data url
            while (data_url$status != 'finished') { #keep checking the status until finished
				if (data_url$status == "error" ) { stop('ALA batch intersect has returned an error; please check your inputs but if the issue continues, please contact package author.') }
                Sys.sleep(5)
                data_url=cached_get(status_url,type="json",caching="off") #get the data url
            }
            download_to_file(data_url$downloadUrl,outfile=this_cache_file,binary_file=TRUE)
        } else {
            ## we are using the existing cached file
            if (verbose) { cat(sprintf("  ALA4R: using cached file %s\n",this_cache_file)) }
        }
        out = read.csv(unz(this_cache_file,'sample.csv'),as.is=TRUE) #read in the csv data from the zip file
		out[which(out=='n/a')] = NA
        checks = NULL; for (ii in colnames(out)) { if (all(is.na(out[,ii]))) checks = c(checks,ii) } #identify bad layer IDs
        if (!is.null(checks)) warning(paste(paste(checks,collapse=', '),'are invalid layer ids')) #warn user of bad layer ids
		out[,checks] = NULL #remove bad columns
		if (dim(out)[2]==2) stop('no valid data returned from the layers chosen')
    } else { #get results if just a single location
        url_str = paste(base_url,'intersect/',layers_str,'/',pnts_str,sep='') #define the url string
        url_str=URLencode(url_str) ## should not be needed, but do it anyway
        out = cached_get(url_str,type="json") #get the data
        if (length(out)==0) stop('all layer ids provided were invalid') #nothing returned if no valid IDs provided
        tt = t(out$value); colnames(tt) = out$field 
        out = data.frame(latitude=pnts[1],longitude=pnts[2],tt) # define the output the same as the bulk output
        checks = setdiff(layers,colnames(out)[-c(1,2)]); if (length(checks)>0) warning(paste(paste(checks,collapse=', '),'are invalid layer ids')) #warn user of bad layer ids
    }
    ##deal with SpatialPointsDataFrame
    if (SPdata.frame) { #if output is requested as a SpatialPointsDataFrame
        ## coerce to SpatialPointsDataFrame class
        if (nrow(out)>0) {
            out=SpatialPointsDataFrame(coords=out[,c("longitude","latitude")],proj4string=CRS("+proj=longlat +ellps=WGS84"),data=out)
        }
    }
	###final formatting before return
    if (use_layer_names) {
        names(out)=make.names(fields_id_to_name(names(out),"layers"))
    }
    names(out)=rename_variables(names(out),type="layers") ## rename vars for consistency
	out[which(out=='n/a')] = NA
	
    ##return the output
    out
}
