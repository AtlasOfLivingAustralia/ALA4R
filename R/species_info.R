#' Fetch a taxon profile given a scientific name or LSID (GUID)
#' 
#' @references Associated ALA web service: \url{http://api.ala.org.au/#ws80}
#' 
#' @param scientificname string: scientific name of the taxon of interest (species, genus, family etc) 
#' @param guid string: The Life Science Identifier of the taxon of interest
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A species profile in the form of a named list, each element of which is generally a data frame. An empty list is returned if no match is found for the supplied name or guid
#' @seealso \code{\link{ala_config}}
#' @examples
#' \dontrun{
#'  species_info("Grevillea humilis subsp. maritima")
#'  species_info(guid="http://id.biodiversity.org.au/node/apni/2890970")
#'  species_info("Alaba",verbose=TRUE)
#' }
#' @export species_info

species_info <- function(scientificname,guid,verbose=ala_config()$verbose) {
    if (!missing(scientificname)) {
        if (is.factor(scientificname)) {
            scientificname <- as.character(scientificname)
        }
        assert_that(is.notempty.string(scientificname))
    }
    if (!missing(guid)) {
        assert_that(is.notempty.string(guid))
    }
    assert_that(is.flag(verbose))
    if (missing(scientificname) && missing(guid)) {
        stop("either the scientific name or the guid must be provided")
    }
    if ((!missing(scientificname)) && (!missing(guid))) {
        stop("either the scientific name or the guid must be provided, but not both")
    }
    if (!missing(scientificname)) {
        guid <- search_names(scientificname,vernacular=FALSE,guids_only=TRUE)
        if (length(guid)<1) {
            if (ala_config()$warn_on_empty) {
                warning("no results found")
            }
            return(list())
        }
        guid <- guid[[1]]
        if (is.na(guid)) {
            if (ala_config()$warn_on_empty) {
                warning("No valid GUID found for scientificname",scientificname)
            }
            return(list())
        }
    }
    this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_bie,c("species",paste0(guid,".json")))
    ## July 2016: invalid GUIDs now return 404 errors. Catch these and return a list()
    out <- tryCatch(cached_get(URLencode(this_url),type="json",verbose=verbose),
              error=function(e) if (grepl("code 404 received",e$message)) NULL else stop(e))
    if (is.null(out)) {
        ## invalid guids will give NULL here, catch them now
        if (ala_config()$warn_on_empty) {
            warning("no results found")
        }
        return(list())
    }
    ## restructure any list children of out to be data.frames
    for (k in 1:length(out)) {
        if (is.list(out[[k]])) {
            was_ok <- FALSE
            try({out[[k]] <- as.data.frame(out[[k]],stringsAsFactors=FALSE); was_ok <- TRUE },silent=TRUE)
            ## that will fail if any children are NULL
            if (!was_ok) {
                try(out[[k]] <- as.data.frame(lapply(out[[1]],function(z)if(is.null(z)) NA else z)),silent=TRUE)
            }
        }
    }
    ## weed out unwanted columns and rename variables in each child object
    ## some columns consistently used across these child objects but seem to be internal identifiers and of little use here
    dud_cols <- c("id","parentId","infoSourceId","documentId")
    for (k in 1:length(out)) {
        if (is.data.frame(out[[k]])) {
            tempcols <- setdiff(names(out[[k]]),dud_cols)
            out[[k]] <- out[[k]][,tempcols,drop=FALSE]
        }
    }
    ## taxonConcept
    if (any(names(out)=="taxonConcept")) {
        tempcols <- setdiff(names(out$taxonConcept),unwanted_columns(type="general"))
                                        #tempcols <- setdiff(tempcols,c("id","parentId","infoSourceId"))
        out$taxonConcept <- subset(out$taxonConcept,select=tempcols)    
        names(out$taxonConcept) <- rename_variables(names(out$taxonConcept),type="general")
        ## taxonName
        names(out$taxonName) <- rename_variables(names(out$taxonName),type="general")
    }
    ## classification
    if (any(names(out)=="classification")) {
        ## for taxa with improper classification (e.g. species_info(guid='ALA_Caladenia_cardiochila')) this is just a string
        if (is.data.frame(out$classification)) {
            tempcols <- setdiff(names(out$classification),unwanted_columns(type="general"))
            out$classification <- subset(out$classification,select=tempcols)
            names(out$classification) <- str_replace_all(names(out$classification),"^clazz","class")
            names(out$classification) <- rename_variables(names(out$classification),type="general")
        } else if (is.character(out$classification)) {
            ## leave as is
        } else {
            warning("the structure of the $classification object was unexpected. ",getOption("ALA4R_server_config")$notify)
        }
    }
    ## identifiers is a list - is OK
    ## commonNames - just the dud_cols above
    #tempcols <- setdiff(names(out$commonNames),c("infoSourceId","documentId"))
    #out$commonNames <- out$commonNames[,tempcols]
    ## synonyms
    if (any(names(out)=="synonyms")) {
        tempcols <- setdiff(names(out$synonyms),unwanted_columns(type="general"))    
        ##tempcols <- setdiff(tempcols,c("id","infoSourceId"))
        out$synonyms <- subset(out$synonyms,select=tempcols)
        names(out$synonyms) <- rename_variables(names(out$synonyms),type="general")
    }
    ## sameAsConcepts
    if (any(names(out)=="sameAsConcepts")) {
        ##tempcols <- setdiff(names(out$sameAsConcepts),c("id"))
        ##out$sameAsConcepts <- out$sameAsConcepts[,tempcols]
        names(out$sameAsConcepts) <- rename_variables(names(out$sameAsConcepts),type="general")
    }
    ## all other child objects seem OK at the moment
    ## we could in principle run everything through the rename/remove columns functions
    ## but let's not do so for now
    out
}
