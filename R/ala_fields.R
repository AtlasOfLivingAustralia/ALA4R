## Retrieve a list of all fields. For occurrence searching, http://biocache.ala.org.au/ws/index/fields. For searching searching taxon, datasets, layers, collections metadata, http://bie.ala.org.au/admin/indexFields

ala_fields=function(fields_type="general") {
    fields_type=tolower(fields_type)
    match.arg(fields_type,c("general","occurrence"))
    base_url="http://bie.ala.org.au/admin/indexFields"
    if (identical(fields_type,"occurrence")) {
        base_url="http://biocache.ala.org.au/ws/index/fields"
    }
    x=GET(url=base_url,user_agent(ala_config()$user_agent)) ## don't bother caching this, just get it each time
    rbind.fill(lapply(content(x),as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
}
