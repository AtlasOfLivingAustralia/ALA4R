clean_string <-
function(x) {
x = gsub("[^[:alpha:] ]", "", x) #remove anything but alpha characters
x = gsub("\\s*$","",x) #remove trailing white space
x = gsub("^ *","",x) #remove leading white space
x = gsub('\\s','_',x) #replace white space with _
while(length(grep('__',x))>0) x = gsub('__','_',x) #remove double __
return(x)
}
