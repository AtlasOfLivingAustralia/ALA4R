\name{search_layers}
\alias{search_layers}
\title{Search for environmental and contextual data layers}
\usage{
search_layers(type = "all", query)
}
\arguments{
  \item{type}{string: either "all" (all possible layers;
  default), "grids" (gridded environmental layers), or
  "shapes" (contextual shapefile layers)}

  \item{query}{text string: optional search term against
  layer metadata. Only layers that include this term in
  their metadata will be returned.}
}
\value{
A data frame of results. The contents (column names) of the
data frame will vary depending on the details of the search
and the results.
}
\description{
Search for environmental and contextual data layers
}
\examples{
\dontrun{
search_layers(type="all")
search_layers(type="grids",query="income")
l=search_layers(type="shapes",query="coral")
str(l)
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
\url{http://spatial.ala.org.au/layers}
}

