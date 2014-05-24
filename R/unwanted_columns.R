## define column names that we will remove from the results because we don't think they will be useful in the ALA4R context
unwanted_columns=function(type) {
    type=match.arg(tolower(type),c("general","layers"))
    switch(type,
           "general"=c("rankId","left","right","idxType","nameComplete","hasChildren","highlight","linkIdentifier","isExcluded"),
           "layers"=c("pid","path","path_orig","enabled","uid","licence_level","lookuptablepath","mdhrlv"),
           c("")
           )
}
