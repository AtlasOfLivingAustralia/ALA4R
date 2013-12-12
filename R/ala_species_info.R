ala_species_info <-
function(taxon) {
taxon = ' 3red  kangaroo\n'
taxon = clean_string(taxon) #clean up the taxon name
out = fromJSON(file=paste("http://bie.ala.org.au/ws/search.json?q=",taxon,sep="")) #download the data




}
