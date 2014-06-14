## define column names that we will remove from the results because we don't think they will be useful in the ALA4R context
unwanted_columns=function(type) {
    type=match.arg(tolower(type),c("general","layers"))
    switch(type,
           "general"=c("rankId","left","right","idxType","highlight","linkIdentifier","isExcluded","hasChildren"),
             ## hasChildren seems always to be false, even for taxa that ought to have children (e.g. Macropus)
           "layers"=c("pid","path","path_orig","path_1km","enabled","uid","licence_level","lookuptablepath","mdhrlv","mddatest","grid","shape"),
             ## datalang appears to be all "eng" "Eng" "enu" "" or NA (2x"enu" records appear to be in English and from DEH/DEWHA)
             ## grid is redundant: all env layers are grid==TRUE, all contextual layers are grid==NA
             ## ditto for shape: all contextual are TRUE, all grid are NA
             ## mddatest is an internal metadata testing date of some sort? 
           c("")
           )
}
