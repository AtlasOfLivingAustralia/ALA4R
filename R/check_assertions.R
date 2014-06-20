#' Check assertions in occurrences object
#'
#' This provides a data.frame detailing the assertions that are found in a dataset returned from \code{\link{occurrences}}.
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' 
#' @param x list: an object returned from \code{\link{occurrences}}
#' 
#' @return A dataframe of assertions column names, descriptions and categories/error codes.
#'
#' @examples
#' #download species data with all possible assertions
#' x=occurrences(taxon="golden bowerbird",download_reason_id=10,qa=ala_fields('assertions')$name)
#' (asserts = check_assertions(x)) #data.frame of assertions, their description and column names
#' @export
check_assertions = function(x) {
    if (any(class(x)=='occurrences')) {
        cois = colnames(x$data) #get the column names
        ass = ala_fields('assertions') #get the assertions
        ass$occurColnames = NA
        for(coi in cois) {
            tt=which(coi==ass$name | coi==rename_variables(ass$description,type="assertions"))
            ## old code for previous variable names
            ## tt = c(grep(tocamel(coi),tocamel(ass$name)),grep(tocamel(coi),tocamel(ass$description))) #check if in name or description columns
            if (length(tt)==0) {
                next
            } else {
                ass$occurColnames[tt[1]] = coi #place the colname
            }
        }
        na.omit(ass)
    } else {
        stop('check_assertions must have an object of class occurrences from e.g., occurrences() in the ALA4R package')
    }
}
