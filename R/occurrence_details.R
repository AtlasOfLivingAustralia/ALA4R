#' Retrieve the full details of occurrence records
#'
#' Note that this makes a separate web request for each occurrence uuid, and so may not be wise to use on a large number of uuids.
#' 
#' @references Associated ALA web service: \url{http://api.ala.org.au/#ws102}
#' 
#' @param uuid string: one or more record ids, as returned by \code{\link{occurrences}} (in the \code{data$id} column)
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A named list (named by uuid), each element of which is a list containing the details for that uuid. This inner list will be empty if no match is found for the supplied uuid
#' @seealso \code{\link{occurrences}} \code{\link{ala_config}}
#' @examples
#' \dontrun{
#' s1 <- occurrence_details("f259c5ce-200c-41a2-b73a-e36a91f748f7")
#' str(s1,max.level=3)
#' }
#' @export occurrence_details

occurrence_details <- function(uuid,verbose=ala_config()$verbose) {
    if (!missing(uuid)) {
        assert_that(is.character(uuid))
    } else {
        stop("uuid must be provided")
    }
    assert_that(is.flag(verbose))
    non_empty <- nchar(uuid)>0 & !is.na(uuid)
    this_url <- sapply(uuid[non_empty],function(z)build_url_from_parts(getOption("ALA4R_server_config")$base_url_biocache,paste0("occurrence/",z)))
    out_non_empty <- lapply(this_url,function(z)cached_get(z,type="json",verbose=verbose))
    out <- vector("list",length(uuid))
    #if (is.null(out)) {
    #    ## invalid guids will give NULL here, catch them now
    #    if (ala_config()$warn_on_empty) {
    #        warning("no results found")
    #    }
    #    return(list())
                                        #}
    non_empty_which <- which(non_empty)
    for (k in 1:length(uuid)) {
        if (non_empty[k]) {
            out[k] <- out_non_empty[non_empty_which[k]]
            if (is.null(out[[k]])) {
                out[[k]] <- list()
            } else {
                ## some renaming of variables
                if (!is.null(out[[k]]$processed$classification)) {
                    try({
                        tempcols<-setdiff(names(out[[k]]$processed$classification),unwanted_columns(type="general"))
                        out[[k]]$processed$classification<-out[[k]]$processed$classification[tempcols]
                        names(out[[k]]$processed$classification)<-str_replace_all(names(out[[k]]$processed$classification),"^classs","class")
                        names(out[[k]]$processed$classification)<-rename_variables(names(out[[k]]$processed$classification),type="general")
                    },silent=TRUE)
                }
            }
        } else {
            out[[k]] <- list()
        }
    }
    names(out) <- uuid    
    out
}
