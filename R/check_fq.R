## checking of fields in fq against valid fields
## not exported for users: internal ALA4R use only
check_fq=function(fq,type) {
    assert_that(is.character(fq))
    assert_that(is.string(type))
    type=match.arg(tolower(type),c("general","occurrence","layers"))
    temp=paste("",fq,collapse=" ") ## with leading space
    temp=gsub("[\\(\\)]+"," ",temp) ## drop all brackets (we don't care about the logic, just the field names that are present) and replace with spaces
    field_names=str_split(temp,"\\s+") ## split on spaces
    field_names=unlist(str_extract_all(unlist(field_names),perl("^[^\\:]+(?=\\:)"))) ## extract "field" in field:value
    field_names=gsub("^\\-","",field_names) ## remove leading "-" characters which can be used to negate fq queries
    valid_fields=ala_fields(type)
    invalid_fields=setdiff(field_names,valid_fields$name)
    if (length(invalid_fields)>0) {
        stop("invalid fields in fq: ",paste(invalid_fields,collapse=", "),". See ala_fields(\"",type,"\")")
    }
}
    
