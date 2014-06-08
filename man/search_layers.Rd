\name{search_layers}
\alias{search_layers}
\title{Search for environmental and contextual data layers}
\usage{
search_layers(query, type = "all", output_format = "simple")
}
\arguments{
  \item{type}{string: either "all" (all possible layers;
  default), "grids" (gridded environmental layers), or
  "shapes" (contextual shapefile layers)}

  \item{query}{text string: optional search term against
  layer metadata. Only layers that include this term in
  their metadata will be returned.}

  \item{output_format}{string: controls the print method
  for the returned object. Either "complete" (the complete
  data structure is displayed), or "simple" (a simplified
  version is displayed). Note that the complete data
  structure exists in both cases: this option only controls
  what is displayed when the object is printed to the
  console. The default output format is "simple"}
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

