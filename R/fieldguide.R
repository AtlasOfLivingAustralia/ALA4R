#' Generate a PDF field guide using the ALA's field guide generator
#'
#' @references \url{http://fieldguide.ala.org.au/}
#' @param guids character: vector of GUIDs 
#' @param title string: title to use in the field guide PDF 
#' @param filename string: filename for the PDF document
#' @param overwrite logical: overwrite the file if it already exists?
#'
#' @return filename
#'
#' @seealso \code{\link{search_guids}}
#'
#' @examples
#' \dontrun{
#' fieldguide(guids=
#'   c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59",
#'   "http://id.biodiversity.org.au/node/apni/2890970"))
#' }
#'
#' @export
fieldguide <- function(guids,title="Field guide",filename=tempfile(fileext=".pdf"),overwrite=FALSE) {
    if (missing(guids)) stop("one or more GUIDs must be supplied")
    if (is.list(guids)) guids <- unlist(guids)
    if (is.factor(guids)) guids <- as.character(guids)
    assert_that(is.character(guids))
    assert_that(is.string(title))
    assert_that(is.flag(overwrite))
    assert_that(is.string(filename))
    if (!overwrite && file.exists(filename)) stop("file exists: delete it or specify overwrite=TRUE")

    this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_fieldguide,"generate")
    temp <- jsonlite::toJSON(list(title=title,guids=guids))
    x <- POST(url=this_url,body=temp,user_agent(ala_config()$user_agent),encode="json")
    ## x response should have header with Fileid: 30082011/fieldguide1314682018564.pdf
    temp <- headers(x)
    if (!is.null(headers(x)$fileid)) {
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_fieldguide,paste0("guide/",headers(x)$fileid))
        tmpfile <- cached_get(this_url,type="binary_filename")
        ok <- file.copy(tmpfile,filename,overwrite=overwrite)
        if (ok) {
            filename
        } else {
            warning("failed to copy temporary file to ",filename)
            NULL
        }
    } else {
        warning("The field guide generator failed. ",getOption("ALA4R_server_config")$notify)
        NULL
    }
}
