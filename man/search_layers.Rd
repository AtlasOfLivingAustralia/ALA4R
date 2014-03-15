\name{search_layers}
\alias{search_layers}
\title{Provides a list of all environmental and contextual layers in ALA}
\usage{
search_layers(type = "all", query = NULL)
}
\arguments{
  \item{type}{string: a selection of "all", "grids" or
  "shapes" associated with all possible layers, only
  environmental grids or contextual shapefiles}

  \item{query}{string: optional search term. Only results
  with this term in the name or description will be
  returned}
}
\value{
A data frame of results \itemize{ \item{name} \item{id}
\item{type} \item{path} \item{description} \item{source}
\item{displayname} \item{enabled} \item{uid}
\item{metadatapath} \item{classification1}
\item{classification2} \item{notes} \item{source_link}
\item{licence_link} \item{licence_notes} \item{maxlatitude}
\item{minlatitude} \item{minlongitude} \item{maxlongitude}
\item{pid} \item{shape} \item{path_orig}
\item{environmentalvalueunits} \item{scale}
\item{environmentalvaluemax} \item{environmentalvaluemin}
\item{lookuptablepath} \item{citation_date} \item{datalang}
\item{licence_level} \item{mddataset} \item{mdhrlv}
\item{respparty_role} \item{keywords} \item{domain} }
}
\description{
Provides a list of all environmental and contextual layers
in ALA
}
\examples{
\dontrun{
search_layers(type="all")
search_layers(type="grids",query="coral")
search_layers(type="shapes")
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
\url{http://spatial.ala.org.au/layers}
}

