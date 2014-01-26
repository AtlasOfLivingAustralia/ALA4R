## Retrieve a list of all fields. For occurrence searching, http://biocache.ala.org.au/ws/index/fields. For searching searching taxon, datasets, layers, collections metadata, http://bie.ala.org.au/ws/admin/indexFields

ala_fields=function(fields_type="general") {
    fields_type=tolower(fields_type)
    match.arg(fields_type,c("general","occurrence"))
    base_url="http://bie.ala.org.au/ws/admin/indexFields"
    if (identical(fields_type,"occurrence")) {
        base_url="http://biocache.ala.org.au/ws/index/fields"
    }
    
    if (identical(ala_config()$caching,"off")) {
        ## if we are not caching, get this directly without saving to file at all
        x=GET(url=base_url,user_agent(ala_config()$user_agent))
        x=content(x)
    } else {
        ## use caching
        thisfile=ala_download_to_file(base_url)
        x=fromJSON(file=thisfile)
    }
    x=rbind.fill(lapply(x,as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
    ## convert factors to strings
    for (col in 1:ncol(x)) {
        if (identical(class(x[,col]),"factor")) {
            x[,col]=as.character(x[,col])
        }
    }
    x
}
