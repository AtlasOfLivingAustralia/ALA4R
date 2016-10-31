#' Quick geographic plot of occurrence data
#' 
#' Generates a plot of occurrence data retrieved using \code{\link{occurrences}}.The plot uses
#' an Australian basemap and colours the occurrence records dots according to parameters
#' 
#' @references \url{http://api.ala.org.au/}
#' 
#' @param x list: a list object that has been downloaded using \code{\link{occurrences}}
#' @param filename string: name of file to be created; defaults to RPlots.pdf
#' @param qa string vector: list of record issues to be mapped; these can be assertion columnnames, 
#' or 'all' or 'none' or any combination of 'error', 'warning' or 'fatal'. Column or categories in 
#' your dataset can be viewed using \code{check_assertions}. 
#' @param grouped logical: TRUE creates a single plot for all observations; FALSE plots individual 
#' maps for the taxon level defined.
#' @param taxon_level string: taxonomic level at which to create maps; possible values are 'species', 
#' 'genus', 'family' or 'order'
#' @param pch single number or character representing point type. See description of \code{pch} in \code{\link{points}}.
#' @param cex numeric: character (or symbol) expansion. See description of \code{cex} in \code{\link{points}}.
#' @param \dots : other options passed to pdf()
#' @return Generates a pdf that maps the distributions.
#' 
#' @examples
#' \dontrun{ 
#' #download some observations
#' x <- occurrences(taxon="Eucalyptus gunnii",download_reason_id=10)
#' occurrences_plot(x)
#' x <- occurrences(taxon="Cider Gum",download_reason_id=10)
#' occurrences_plot(x,"alaPlot.pdf",qa="fatal",grouped=FALSE, taxon_level="species",pch='+')
#' }
#' @export occurrences_plot
occurrences_plot <- function(x, filename='Rplots.pdf', qa=c('fatal','error'), grouped=FALSE, taxon_level='species', pch, cex=0.75, ...) 
{
	if (! inherits(x,"occurrences")) stop("occurrences_plot must have an object of class occurrences from e.g., ",getOption("ALA4R_server_config")$occurrences_function,"() in the ",getOption("ALA4R_server_config")$brand," package")
        assert_that(is.string(taxon_level))
        taxon_level <- match.arg(tolower(taxon_level),c("species","genus","family","order"))
	assert_that(is.notempty.string(filename))
	if (substr(filename,nchar(filename)-2,nchar(filename))!='pdf') filename <- paste(filename,'.pdf',sep='') #append a pdf suffix to filename
	assert_that(is.flag(grouped))
	assert_that(is.character(qa))
        assert_that(is.scalar(cex))
	if (missing('pch')) {
		pch <- 19
	} else {
		if (length(pch)>1) { pch <- pch[1]; warning('only using first element of supplied pch vector') }
		if (nchar(pch)>1) { pch <- substr(pch,1,1); warning('only using first character of supplied pch text') }
	}
	ass <- check_assertions(x)
	if ('none' %in% qa) { 
		qa <- NULL
	} else {
		tt <- NULL
		if ('all' %in% qa) tt <- c(tt,ass$occurColnames)
		if ('error' %in% qa) tt <- c(tt,ass$occurColnames[which(ass$category=='error')])
		if ('warning' %in% qa) tt <- c(tt,ass$occurColnames[which(ass$category=='warning')])
		if ('fatal' %in% qa) tt <- c(tt,ass$occurColnames[which(as.logical(ass$fatal)==TRUE)])
		if (any(qa %in% colnames(x$data))) {
			valid_fields <- ass$occurColnames ## valid entries for qa
			unknown <- setdiff(qa,valid_fields)
			if (length(unknown)>0) {
				warning("invalid qa fields requested: ", str_c(unknown,collapse=", "), ". See ",getOption("ALA4R_server_config")$fields_function,"(\"assertions\",as_is=TRUE)")
			}
			tt <- intersect(qa,valid_fields)
		}
		tt <- intersect(tt,colnames(x$data))
		if (length(tt)>0) { qa <- tt } else { qa <- NULL }
    }

        ## load aus map data
        ##note this will ideally be states
        
        ## don't do this: the aus variable isn't visible to check()
        ##data("aus",envir = environment())
	
        load_obj <- function(f) { env <- new.env(); nm <- load(f, env)[1]; env[[nm]] }
        aus <- load_obj(system.file("data/aus.RData",package="ALA4R"))        
	
	###plot function to be used
	tplot <- function(xx,Main,coi,pch) {
		image(aus,col='grey') #draw the base australia
		title(main=Main)
		degAxis(1); degAxis(2) #add on the axis
		points(xx$longitude,xx$latitude,pch=pch,col='black')
		if (is.null(coi)) {
			legend('bottomleft',legend='assumed good',pch=pch,col='black',bty='n',cex=cex)
		} else {
			legend.cols <- rainbow(length(coi)) #define the legend colors
			c2use <- NULL #define columns to keep because they had issues
			for (ii in 1:length(coi)) {
				roi <- which(as.logical(xx[,coi[ii]])==TRUE) #define the points that have the issue
				if (length(roi) > 0) {
					points(xx$longitude[roi],xx$latitude[roi],pch=pch,col=legend.cols[ii])
					c2use <- c(c2use,ii)
				}				
			}
			if (is.null(c2use)) {
				legend('bottomleft',legend='assumed good',pch=pch,col='black',bty='n',cex=cex)
			} else {
				legend('bottomleft',legend=c('assumed good',coi[c2use]),pch=pch,col=c('black',legend.cols[c2use]),bty='n',cex=cex)
			}
		}
	}
	
	###generate the plots
	pdf(filename,...)
        ## wrap plotting code in tryCatch block so that device will be closed cleanly on error
        tryCatch({
			if (grouped) {
				tplot(x$data,Main='all species',coi=qa,pch)
			} else {
				cat('this is plotting',length(unique(x$data[,taxon_level])),taxon_level,'maps... names will act as status bar\n')
				spp_count <- 0
				for (spp in unique(x$data[,taxon_level])) {
					spp_count <- spp_count+1
					cat(spp_count,'.\t',spp,'\n',sep="")
					if (spp!="") { 
						tplot(x$data[which(x$data[,taxon_level]==spp),],Main=spp,coi=qa,pch)
					} else {
						tplot(x$data[which(x$data[,taxon_level]==spp),],Main='unmatched species name',coi=qa,pch)
					}
				}
			}
		}, error=function(e) { dev.off(); unlink(filename); stop(e) })
	dev.off()
    invisible(0) ## return nothing
}


