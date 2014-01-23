require("httr")
require("plyr")
require("rjson")

ala_species_info <- function(taxon) {
    ## grab all data for a taxon of interest
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon)
        x=GET(url=build_url(this_url))
        content(x)[[1]]
}


ala_autocomplete=function(taxon,limit=10) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search/auto.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon,limit=limit)
        x=GET(url=build_url(this_url))
        x=content(x)[[1]]
        ## data comes back as nested list structure, which is a direct and somewhat mindless conversion of the raw JSON data
        ## some variables are returned as lists, and these may be empty (e.g. commonNameMatches, if there are no such matches) which causes problems when casting to data.frame
        ldply(x,function(y){ as.data.frame(lapply(y,function(z){ ifelse(class(z)=="list" && length(z)==0,"",z) })) })
    }
        

##Response [http://bie.ala.org.au/ws/search/auto.json?q=macropus&limit=10]
##  Status: 200
##  Content-type: application/json;charset=UTF-8
##{"autoCompleteList":[{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:9e6a0bba-de5b-4465-8544-aa8fe3943fab","name":"Macropus","occurrenceCount":9405,"georeferencedCount":9405,"scientificNameMatches":["<b>Macropus</b>"],"matchedNames":["Macropus"],"rankId":6000,"rankString":"genus","left":32346,"right":32403},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:c42a3e19-eeff-4282-8366-ab3ee2f4f73c","name":"Macropus greyi","occurrenceCount":11,"georeferencedCount":11,"scientificNameMatches":["<b>Macropus</b> greyi"],"commonNameMatches":[],"commonName":"Toolache Wallaby, Tooloche Wallaby","matchedNames":["Macropus greyi"],"rankId":7000,"rankString":"species","left":32349,"right":32350},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:4df3511b-0bd0-4e57-83e0-4a267b5a6306","name":"Macropus dorsalis","occurrenceCount":34,"georeferencedCount":34,"scientificNameMatches":["<b>Macropus</b> dorsalis"],"commonNameMatches":[],"commonName":"Black- Striped Wallaby, Black-striped Wallaby","matchedNames":["Macropus dorsalis"],"rankId":7000,"rankString":"species","left":32373,"right":32374},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:d469add5-f4f6-4271-a48f-f67cca445a07","name":"Macropus antilopinus","occurrenceCount":27,"georeferencedCount":27,"scientificNameMatches":["<b>Macropus</b> antilopinus"],"commonNameMatches":[],"commonName":" Antilopine Kangaroo,  Antilopine Wallaby, Antilopine Kangaroo, Antilopine Wallaroo","matchedNames":["Macropus antilopinus"],"rankId":7000,"rankString":"species","left":32389,"right":32390},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:9d5d9e5b-4374-4129-aa9a-ff7921f3b1c9","name":"Macropus robustus","occurrenceCount":89,"georeferencedCount":89,"scientificNameMatches":["<b>Macropus</b> robustus"],"commonNameMatches":[],"commonName":"Common Wallaroo","matchedNames":["Macropus robustus"],"rankId":7000,"rankString":"species","left":32375,"right":32384},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:fe69ffcc-a108-4675-a25c-f4428203e4de","name":"Macropus irma","occurrenceCount":2,"georeferencedCount":2,"scientificNameMatches":["<b>Macropus</b> irma"],"commonNameMatches":[],"commonName":" Kwoora,  Western Bush Wallaby, Western Brush Wallaby","matchedNames":["Macropus irma"],"rankId":7000,"rankString":"species","left":32347,"right":32348},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:ec13cfac-4783-46c2-9ad3-50e2c60f1348","name":"Macropus bernardus","occurrenceCount":12,"georeferencedCount":12,"scientificNameMatches":["<b>Macropus</b> bernardus"],"commonNameMatches":[],"commonName":" Bernard's Wallaroo, Black Wallaroo, Woodward's Wallaroo","matchedNames":["Macropus bernardus"],"rankId":7000,"rankString":"species","left":32385,"right":32386},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:102c2bde-cebf-4de7-9cfd-79c68a0b54bb","name":"Macropus rufogriseus","occurrenceCount":1055,"georeferencedCount":1055,"scientificNameMatches":["<b>Macropus</b> rufogriseus"],"commonNameMatches":[],"commonName":" Bennett's Wallaby, Brush Wallaby, Brush Kangaroo, Brusher, Red Wallaby., Necked Wallaby, Red-necked Wallaby, Red-necked Wallaby And Joey","matchedNames":["Macropus rufogriseus"],"rankId":7000,"rankString":"species","left":32397,"right":32402},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:6e8e49d5-da08-4567-9b18-429bd6a53237","name":"Macropus fuliginosus","occurrenceCount":2139,"georeferencedCount":2139,"scientificNameMatches":["<b>Macropus</b> fuliginosus"],"commonNameMatches":[],"commonName":" Kangaroo Island Western Grey Kangaroo,  Western Grey Kangaroo, Males Are Known As Stinkers Due To Their Strong, Curry-like Smell., Western Gray Kangaroo, Western Grey Kangaroo","matchedNames":["Macropus fuliginosus"],"rankId":7000,"rankString":"species","left":32391,"right":32396},{"guid":"urn:lsid:biodiversity.org.au:afd.taxon:53026a84-c638-4ea7-a498-dcbf611f3173","name":"Macropus eugenii","occurrenceCount":274,"georeferencedCount":274,"scientificNameMatches":["<b>Macropus</b> eugenii"],"commonNameMatches":[],"commonName":" Tamar Wallaby, Dama Wallaby, Tammar Wallaby, Tammar, Dama Wallaby","matchedNames":["Macropus eugenii"],"rankId":7000,"rankString":"species","left":32365,"right":32372}]} 


ala_bulklookup=function(taxa=list()) {
	taxa = lapply(taxa,clean_string) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/species/bulklookup.json"
        x=POST(url=base_url,body=toJSON(taxa))        
        rbind.fill(lapply(content(x)[[1]],as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
    }

