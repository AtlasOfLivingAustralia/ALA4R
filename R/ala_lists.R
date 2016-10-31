#' Species lists
#'
#' Note that this refers to pre-generated lists of species stored on the ALA servers. The similarly-named but different function \code{\link{specieslist}} provides a different function, namely listing the species matching a query or recorded as present in a search area.
#'
#' @references \url{http://lists.ala.org.au} and the associated web services at \url{http://lists.ala.org.au/ws}
#' @param druid string: data resource UID of the list (i.e. the list identifier)
#' @param kvp logical: include key-value pairs? Some lists contain information about the species in the form of key-value pairs
#' @param verbose logical: show additional progress information? 
#'
#' @return data.frame
#'
#' @seealso \code{\link{specieslist}} \code{\link{ala_lists}}
#'
#' @examples
#' \dontrun{
#'  all_lists <- ala_lists()
#'  ## find the "Field Guide apps species profiles" from Museum Victoria
#'  all_lists[grep("Field Guide",all_lists$listName),]
#'  ## download the vertebrates one
#'  l <- ala_list(druid="dr1146")
#' }
#'
#' @export
ala_list <- function(druid,kvp=TRUE,verbose=ala_config()$verbose) {
    assert_that(is.string(druid))
    assert_that(is.flag(kvp))
    assert_that(is.flag(verbose))
    valid_ids <- ala_lists()$dataResourceUid
    if (!druid %in% valid_ids) stop(sprintf("the supplied data resource UID (%s) does not appear to be a valid list ID",druid))
    if (kvp) {
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_lists,c("speciesListItems",druid),list(includeKVP="true"))
    } else {
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_lists,c("speciesListItems",druid))
    }
    cached_get(this_url,type="json",verbose=verbose)
}




#' Find ALA species lists
#'
#' @references \url{http://lists.ala.org.au} and the associated web services at \url{http://lists.ala.org.au/ws}
#' @param guid string: (optional) if provided, return only lists in which this GUID appears
#' @param offset integer: the number of lists to skip. This supports paging
#' @param max integer: the maximum number of lists to return. This supports paging
#' @param verbose logical: show additional progress information? 
#'
#' @return data.frame of list name and other details
#'
#' @seealso \code{\link{ala_list}}
#'
#' @examples
#' \dontrun{
#'  ## lists that include the giant African snail Achatina fulica
#'  ##  (which is a notifiable pest species in some states)
#'  l <- ala_lists(search_guids("Achatina fulica")$guid)
#' }
#'
#' @export
ala_lists <- function(guid,offset=0,max=500,verbose=ala_config()$verbose) {
    assert_that(is.flag(verbose))
    if (!missing(guid)) {
        assert_that(is.string(guid))
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_lists,c("species",guid),list(offset=offset,max=max))
        cached_get(this_url,type="json",verbose=verbose)
    } else {
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_lists,"speciesList",list(offset=offset,max=max))
        cached_get(this_url,type="json",verbose=verbose)$lists
    }
}

