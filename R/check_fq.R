## checking of fields in fq against valid fields
## not exported for users: internal ALA4R use only
check_fq=function(fq,type) {
    assert_that(is.character(fq))
    assert_that(is.string(type))
    type=match.arg(tolower(type),c("general","occurrence","layers"))
    temp=paste("",fq,collapse=" ") ## with leading space
    field_names=str_split(temp,"\\s+") ## split on spaces
    field_names=unlist(str_extract_all(unlist(field_names),perl("^[^\\:]+(?=\\:)"))) ## extract "field" in field:value
    valid_fields=ala_fields(type)
    invalid_fields=setdiff(field_names,valid_fields$name)
    if (length(invalid_fields)>0) {
        stop("invalid fields in fq: ",paste(invalid_fields,collapse=", "),". See ala_fields(\"",type,"\")")
    }
}
    
