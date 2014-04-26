## define column names that we will remove from the results because we don't think they will be useful in the ALA4R context
unwanted_columns=function(type) {
    type=match.arg(tolower(type),c("general"))
    if (type=="general") {
        c("rankId","left","right","idxType","nameComplete","hasChildren","highlight","linkIdentifier","isExcluded")
    } else {
        c("")
    }
}
