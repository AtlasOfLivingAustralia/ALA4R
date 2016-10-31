## checking of fields in fq against valid fields
## not exported for users: internal ALA4R use only
check_fq <- function(fq,type) {
    assert_that(is.character(fq))
    assert_that(is.string(type))
    type <- match.arg(tolower(type),c("general","occurrence","layers"))
    if (identical(type,"occurrence")) { type <- "occurrence_indexed" }
    field_names <- extract_fq_fieldnames(fq)
    if (is.null(field_names)) {
        ## no matches, so somehow fq doesn't match our expected syntax
        warning("fq may be invalid. See ",getOption("ALA4R_server_config")$fields_function,"(\"",type,"\",as_is=TRUE) for valid fields and help(\"",getOption("ALA4R_server_config")$occurrences_function,"\") for general help on fq syntax")
    } else {
        valid_fields <- ala_fields(type,as_is=TRUE)
        invalid_fields <- setdiff(tolower(field_names),tolower(valid_fields$name))
        if (length(invalid_fields)>0) {
            warning("there may be invalid fields in fq: ",paste(invalid_fields,collapse=", "),". See ",getOption("ALA4R_server_config")$fields_function,"(\"",type,"\",as_is=TRUE)")
        }
    }
}


extract_fq_fieldnames <- function(fq) {
    assert_that(is.character(fq))
    ## pick out field names as anything after a separator character (e.g. space|bracket|+|-), and followed by a colon
    ## see https://wiki.apache.org/solr/CommonQueryParameters for syntax form
    sepchars <- paste0("[:space:]",paste0("",strsplit("+)(-}{","")[[1]],collapse="\\"))
    field_names <- paste("",fq,collapse=" ") ## collapse into single string and add leading space
    ## need to drop anything inside square brackets: these indicate ranges and can cause problems when pulling out field names
    field_names <- str_replace_all(field_names,"\\[.*?\\]","range")
    field_names <- str_match_all(field_names,paste0("[",sepchars,"]([^:",sepchars,"]+?)[[:space:]]*:"))[[1]]
    if (nrow(field_names)<1) {
        NULL
    } else {
        field_names[,2]
    }
}
