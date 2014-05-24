###DO NOT TEST YET

###filter the occurrences download to remove unwanted data

occurrences_subset = function(x,exclude.spatial=c('warnings','errors','missing'), 
	exclude.temporal=c('warnings','errors','missing'),
	exclude.taxonomic=c('warnings','errors','missing'),
	remove.fatal=TRUE, unique.spatial=0, unique.temporal=FALSE) 
{
	#check x
	#check excludes
	#check remove.fatal
	#check unique.spatial
	#check unique.temporal
	
	ass = check_assertions(x) #need to get existing assertions in occur dataset
	if (nrow(ass)==0) warning('no assertions in occurrence data')
	
	roi = NULL #define an object outlining rows to remove
	for (ii in 1:nrow(ass)) {
		if (ass$fatal==TRUE) {
			if (remove.fatal) { #remove the fatal data
				roi = c(roi, which(x$data[,ass$occur.colnames[1]] == TRUE)); next
			} 
		}
		if (ass$code[ii] < 10000) { #remove data with spatial issues
			if (length(exclude.spatial)>0) {
				if (ass$category[ii] %in% exclude.spatial) {
					roi = c(roi, which(x$data[,ass$occur.colnames[1]] == TRUE)); next
				}
			}
		} else if (ass$code[ii] >= 10000 & ass$code[ii] < 20000) { #remove data with taxonomic issues
			if (length(exclude.taxonomic)>0 ) {
				if (ass$category[ii] %in% exclude.taxonomic) {
					roi = c(roi, which(x$data[,ass$occur.colnames[1]] == TRUE)); next
				}
			}		
		} else if (ass$code[ii] >= 30000) { #remove data with temporal issues
			if (length(exclude.temporal)>0 ) {
				if (ass$category[ii] %in% exclude.temporal) {
					roi = c(roi, which(x$data[,ass$occur.colnames[1]] == TRUE)); next
				}
			}		
		}
	}
	roi = unique(roi) #remove duplicates
	if (length(roi)>0) out = x$data[-roi,] #remove the data
	
	###do unique
	###return smaller subset
	
	out
}
