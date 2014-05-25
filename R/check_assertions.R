### get a list of assertions that are found in dataset returned from occurrences()
## not exported for users: internal ALA4R use only
check_assertions = function(x) {
	if (any(class(x)=='occurrences')) {
	cois = colnames(x$data) #get the column names
	ass = ala_fields('assertions') #get the assertions
	ass$occur.colnames = NA
	for(coi in cois) {
		tt = c(grep(tocamel(coi),tocamel(ass$name)),grep(tocamel(coi),tocamel(ass$description))) #check if in name or description columns
		if (length(tt)==0) {
			next
		} else {
			ass$occur.colnames[tt[1]] = coi #place the colname
		}
	}
	na.omit(ass)
	} else { stop('check_assertions must have an object of class occurrences from e.g., occurrences() in the ALA4R package') }
}
