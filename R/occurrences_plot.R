#' Quick plot of occurrence data
#' 
#' Generates a plot of occurrence data retrieved using \code{\link{occurrences}}.
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' 
#' @param x list: a list object that has been downloaded using \code{occurrences}
#' @param filename string: name of file to be created; defaults to RPlots.pdf
#' @param qa string vector: list of record issues to be mapped; these can be assertion columnnames, or 'all' or 'none' or any combination of 'error', 'warning' or 'fatal'. Column or categories in your dataset can be viewed using \code{check_assertions}. 
#' @param grouped logical: TRUE creates a single plot for all observations; FALSE plots individual maps for the taxon level defined.
#' @param taxon_level string: taxonomic level at which to create maps; possible values are 'species', 'genus', 'family' or 'order'
#' @param \dots : other options passed to pdf()
#' @return Generates a pdf that maps the distributions.
#' 
#' @examples
#' \dontrun{ 
#' #download some observations
#' x=occurrences(taxon="golden bowerbird",download_reason_id=10)
#' occurrences_plot(x)
#' }
#' @export occurrences_plot
occurrences_plot = function(x, filename='Rplots.pdf', qa=c('fatal','error'), grouped=FALSE, taxon_level='species',...) 
{
	if (!any(class(x)=='occurrences')) stop('check_assertions must have an object of class occurrences from e.g., occurrences() in the ALA4R package')
        assert_that(is.string(taxon_level))
        taxon_level=match.arg(tolower(taxon_level),c("species","genus","family","order"))
	assert_that(is.string(filename))
	if (substr(filename,nchar(filename)-2,nchar(filename))!='pdf') filename=paste(filename,'.pdf',sep='') #append a pdf suffix to filename
	assert_that(is.flag(grouped))
	assert_that(is.character(qa))
	ass = check_assertions(x)
	if ('none' %in% qa) { 
		qa = NULL
	} else {
		tt = NULL
		if ('all' %in% qa) tt = c(tt,ass$occurColnames)
		if ('error' %in% qa) tt = c(tt,ass$occurColnames[which(ass$category=='error')])
		if ('warning' %in% qa) tt = c(tt,ass$occurColnames[which(ass$category=='warning')])
		if ('fatal' %in% qa) tt = c(tt,ass$occurColnames[which(as.logical(ass$fatal)==TRUE)])
		if (any(qa %in% colnames(x$data))) {
			valid_fields=ass$occurColnames ## valid entries for qa
			unknown=setdiff(qa,valid_fields)
			if (length(unknown)>0) {
				warning("invalid qa fields requested: ", str_c(unknown,collapse=", "), ". See ala_fields(\"assertions\")")
			}
			tt = intersect(qa,valid_fields)
		}
		tt = intersect(tt,colnames(x$data))
		if (length(tt)>0) { qa = tt } else { qa = NULL }
    }
    	
	###note this will ideally be states
	data(aus) #load('data/aus') #load the data
	
	###plot function to be used
	tplot = function(xx,Main,coi,...) {
		image(aus,col='grey') #draw the base australia
		title(main=Main)
		degAxis(1); degAxis(2) #add on the axis
		points(xx$longitude,xx$latitude,pch=19,col='black')
		if (is.null(coi)) {
			legend('bottomleft',legend='assumed good',pch=19,col='black',bty='n',cex=0.75)
		} else {
			legend.cols = rainbow(length(coi)) #define the legend colors
			c2use = NULL #define columns to keep because they had issues
			for (ii in 1:length(coi)) {
				roi = which(as.logical(xx[,coi[ii]])==TRUE) #define the points that have the issue
				if (length(roi) > 0) {
					points(xx$longitude[roi],xx$latitude[roi],pch=19,col=legend.cols[ii])
					c2use = c(c2use,ii)
				}				
			}
			if (is.null(c2use)) {
				legend('bottomleft',legend='assumed good',pch=19,col='black',bty='n',cex=0.75)
			} else {
				legend('bottomleft',legend=c('assumed good',coi[c2use]),pch=19,col=c('black',legend.cols[c2use]),bty='n',cex=0.75)
			}
		}
	}
	
	###generate the plots
	pdf(filename,...)
		if (grouped) {
			tplot(x$data,Main='all species',coi=qa)
		} else {
			##if (taxon_level %in% c('species','genus','family','order')) { ## this check made with match.arg at top of function
                            grouping=taxon_level
                            ## code for old variable names, left here temporarily
                            ##if (taxon_level=='species') grouping = 'Species...matched'
                            ##if (taxon_level=='genus') grouping = 'Genus...matched'
                            ##if (taxon_level=='family') grouping = 'Family...matched'
                            ##if (taxon_level=='order') grouping = 'Order...matched'
			##} else {
			##	dev.off()
			##	unlink(filename)
			##	stop("taxon_level must be defined as one of 'species', 'genus', 'family' or 'order'")
			##}
			cat('this is plotting',length(unique(x$data[,grouping])),taxon_level,'maps... names will act as status bar\n')
                        spp_count=0
			for (spp in unique(x$data[,grouping])) {
                            spp_count=spp_count+1
				cat(spp_count,'.\t',spp,'\n',sep="")
				if (spp!="") { 
					tplot(x$data[which(x$data[,grouping]==spp),],Main=spp,coi=qa)
				} else {
					tplot(x$data[which(x$data[,grouping]==spp),],Main='unmatched species name',coi=qa)
				}
			}
		}
	dev.off()
}


