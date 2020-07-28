#' Check assertions in occurrences object
#'
#' This provides a data.frame detailing the assertions that are found
#' in a dataset returned from \code{\link{occurrences}}.
#'
#' @references \url{https://api.ala.org.au/},
#' \samp{https://biocache-ws.ala.org.au/ws/assertions/codes}
#'
#' @param x list: an object returned from \code{\link{occurrences}}
#'
#' @return A dataframe of assertions column names, descriptions and
#' categories/error codes. If no assertions are in the dataset, NULL is
#' returned.
#'
#' @examples
#' \dontrun{
#'  ##download species data with all possible assertions
#'
#'  x <- occurrences(taxon="golden bowerbird", download_reason_id=10,
#'  qa=ala_fields("assertions")$name)
#'
#'  asserts <- check_assertions(x)
#'  ## this is a data.frame of assertions, their description and column names
#'
#'  ## list the descriptions of all (current) assertions
#'  asserts$description
#'
#'  ## assertion columns from data
#'  tmp <- x$data[, names(x$data) %in% asserts$name]
#'  which(colSums(tmp) > 0) ## discard those not seen in the data
#' }
#' @export check_assertions

check_assertions <- function(x) {
    if (! inherits(x, "occurrences")) {
        stop("check_assertions must have an object of class occurrences
             from e.g., ",
             getOption("ALA4R_server_config")$occurrences_function,
             "() in the ", getOption("ALA4R_server_config")$brand, " package")
    }
    ass <- ala_fields("assertions", as_is = TRUE) ## get all assertion fields
    ass$occurColnames <- NA
    temp_description <- rename_variables(ass$description, type = "assertions")
    for (coi in colnames(x$data)) {
        ## match on either name or description
        tt <- which(coi == ass$name | coi == temp_description)
        if (length(tt) > 0)
            ass$occurColnames[tt[1]] <- coi #place the colname
    }
    ass <- na.omit(ass)
    if (nrow(ass) < 1) {
        if (ala_config()$warn_on_empty) {
            warning("no assertions in data")
        }
        NULL
    } else {
        ass
    }
}
