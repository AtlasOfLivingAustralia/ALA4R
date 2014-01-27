#' List of fields of the environmental and contextual layers
#' 
#' Used to provide a list of all fields associated with the environmental and contextual layers provided through ALA.
#' 
#' @aliases fields field_info
#' @param field_id field id; the field id value from the \code{field} command
#' @return A data frame of results
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}, Ben Raymond \email{ben@@theraymonds.org}
#' @references \url{http://spatial.ala.org.au/ws/}
#' @examples
#' 
#' fields()
#' field_info('cl22')
#' 
#' @export fields field_info
fields = function() {
    base_url = 'http://spatial.ala.org.au/ws/fields' #define the base url
	out = GET(url=base_url,user_agent(ala_config()$user_agent)) #download all data
	out = content(out) #keep only the content
	do.call('rbind.fill',lapply(out,as.data.frame)) #bind the data as a dataframe
}

field_info = function(field_id) {
    base_url = 'http://spatial.ala.org.au/ws/field' #define the base url
	out = GET(url=paste(base_url,field_id,sep='/'),user_agent(ala_config()$user_agent)) #download all data
	if (out$headers$status != "200") { #check returned status 
		stop('please check list of valid field ids using fields(). If your field id is correct, the ALA service may be down; please try again later.') 
	}
	if (substr(field_id,1,2) == 'cl') {
		out = content(out)$objects #keep only the content
		do.call('rbind.fill',lapply(out,as.data.frame)) #bind the data as a dataframe
	} else if (substr(field_id,1,2) == 'el') {
		out = content(out) #keep only the content
		out = as.data.frame(rbind(out)) #bind the data as a dataframe	
		rownames(out) = NULL #reset the row names
		out
	}
}	
