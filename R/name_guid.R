#' Returns the GUID for a scientific name
#' 
#' Returns the GUID for a scientific name
#' 
#' @param taxon string: the scientific name of interest, or a vector of names, i.e. c("name1","name2")
#' @param guids_only logical: if TRUE, only the GUID for each name is returned (or an empty string if no match). Otherwise, additional information is also returned
#' @param verbose boolean value defining how much progress information to display; default is set by ala_config().
#' @return list: If guids_only is TRUE, a named list of character GUIDs. Otherwise, a named list in which each element is a list giving additional information. The accepted GUID is the acceptedIdentifier list component
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @examples
#' 
#' 	name_guid("Macropus greyi")
#' 	name_guid("Macropus greyi",guids_only=FALSE)
#' 	name_guid(c("Macropus greyi","Pachyptila turtur","thisisnotavalidname"))
#' 
#' @export name_guid
#'

## ISSUES: for single name, /ws/guid returns an un-named list of length 1, of which the first (only) item is a single result in the form of a named list (names of fields are e.g. identifier, acceptedName, etc
## whereas for the batch version /ws/guid/batch, we get a named list of length N_names, each entry of which is a list (which seems to be limited to length 1, but perhaps may be longer in some cases), each entry of which is a named list as above
## is it possible for these latter lists to be of length >1?



name_guid=function(taxon,guids_only=TRUE,verbose=ala_config()$verbose) {
    if (! identical(class(taxon),"character")) {
        stop("expecting string or vector of strings as input")
    }
    if (any(nchar(taxon)<1)) {
        stop("input contains empty string")
    }
    if (length(taxon)<1) {
        stop("empty input")
    }
    if (length(nchar(taxon))==1) {
        ## a single name has been provided
	taxon = str_trim(taxon) ## remove leading and trailing whitespaces
	safe_taxon = gsub('\\s+','%20',taxon) ## replace multiple whitespaces with single url-encoded one
        if (verbose) { cat(sprintf("  ALA4R: requesting GUID for name \"%s\"\n",taxon)) }
        base_url=paste(ala_config()$base_url_bie,"guid/",safe_taxon,sep="")
        out=cached_get(base_url,verbose=verbose,type="json")
        if (identical(find("fromJSON"),"package:jsonlite")) {
            if (!is.null(out)) {
                if (guids_only) {
                    out=list(out$acceptedIdentifier)
                    names(out)=taxon
                }
            }
        } else {
            if (length(out)>0) {
                out=setNames(out,taxon) ## create named list
                if (guids_only) {
                    out=lapply(out,function(z){z$acceptedIdentifier})
                }
            }
        }
        ## note: it's not clear whether we should ever expect out to be a list of more than one element. Presumably this is possible if we get two matches on the same scientific name
        out
    } else {
        ## use bulk lookup
	taxon = str_trim(taxon) ## remove leading and trailing whitespaces
	taxon_safe = gsub('\\s+','%20',taxon) ## replace multiple whitespaces with single url-encoded one
        base_url=paste(ala_config()$base_url_bie,"guid/batch",sep="")
        ## this presumably *should* be called in the form e.g. guid/batch?q=name1,name2
        ## that does not work (bug to be lodged)
        ## for now, this works and is used here: guid/batch?q=name1&q=name2
        this_url=paste(base_url,"?q=",str_c(taxon_safe,collapse="&q="),sep="")
        if (verbose) {
            cat(sprintf("  ALA4R: requesting GUIDs for %d names: \"%s\"\n",length(taxon),str_c(taxon,collapse="\",\"")))
            cat(sprintf("    url: %s\n",this_url))
        }
        out=cached_get(this_url,verbose=verbose,type="json")
        ## each list entry is either an empty list (for un-matched names) or a list of 1 list for matched names
        ## again, not clear if this latter can ever be a list of length >1
        if (guids_only) {
            if (identical(find("fromJSON"),"package:jsonlite")) {
                out=lapply(out,function(z){ ifelse(length(z)>0,z$acceptedIdentifier,"") })
            } else {
                out=lapply(out,function(z){ ifelse(length(z)>0,z[[1]]$acceptedIdentifier,"") })
            }
        }
        out
    }
}
