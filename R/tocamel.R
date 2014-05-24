###convert to camel case ... modified from help forum example
## not exported for users: internal ALA4R use only
tocamel = function (x, delim = "[^[:alnum:]]", upper = FALSE, sep = "") {
    assertthat(is.character(x))
    assertthat(is.string(delim))
    s <- strsplit(x, delim, ...)
    sapply(s, function(y) {
        if (any(is.na(y))) {
            y
        }
        else {
            first <- substring(y, 1, 1)
            if (isTRUE(upper)) 
                first <- toupper(first)
            else first[-1] <- toupper(first[-1])
            paste(first, substring(y, 2), sep = "", collapse = sep)
        }
    })
}
