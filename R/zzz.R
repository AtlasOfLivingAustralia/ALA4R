.onLoad <- function(lib, pkg)  {
	
	pkg.info <- utils::packageDescription('ALA4R') 
	packageStartupMessage(paste("ALA4R ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))

	return(invisible(0))
}
