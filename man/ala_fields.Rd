\name{ala_fields}
\alias{ala_fields}
\alias{field_info}
\title{Retrieves a list of all field names that can be used with data retrieval functions}
\usage{
ala_fields(fields_type = "occurrence")

field_info(field_id)
}
\arguments{
  \item{fields_type}{text: either \itemize{ \item "general"
  - for searching taxa, datasets, layers, and collections
  metadata \item "occurrence" - for searching species
  occurrence records \item "layers" - fields associated
  with the environmental and contextual layers. For
  additional information on layers, including metadata and
  licensing, see \code{\link{search_layers}} \item
  "assertions" - potential issues flagged on one or more
  occurrence record fields }}

  \item{field_id}{text: id of environmental/contextual
  layer field for which to look up information Prepend "el"
  for "environmental" (gridded) layers and "cl" for
  "contextual" (polygonal) layers}
}
\value{
A data frame containing the field names and various
attributes
}
\description{
Retrieves a list of all field names that can be used with
data retrieval functions
}
\examples{
l=ala_fields("layers")
l[1,]
a=ala_fields("assertions")
a[,5]
field_info("cl22")
field_info("el773")
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\itemize{ \item ALA web service API:
\url{http://api.ala.org.au/} \item Descriptions of the
spatial layers: \url{http://spatial.ala.org.au/layers/} }
}
\seealso{
\code{\link{search_layers}} to search for spatial layers
}

