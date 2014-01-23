ala_user_agent=function() {
    ## returns a default suitable user-agent string
    version_string="version unknown"
    suppressWarnings(
        try( version_string<-utils::packageDescription('ALA4R')[["Version"]],silent=TRUE) ## get the ALA4R version, if we can
        )
    paste("ALA4R ",version_string," (",R.Version()$version.string,"/",R.Version()$platform,")",sep="")
}
