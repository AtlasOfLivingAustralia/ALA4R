clean_string <- function(x) {
	x = gsub("[^[:alpha:]\\. ]", "", x) #remove anything but alpha characters
	x = str_trim(x) ## remove leading and trailing whitespaces
	x = gsub('\\s+',' ',x) ## replace multiple whitespaces with single
	x
}
