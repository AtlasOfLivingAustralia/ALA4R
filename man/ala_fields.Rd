\name{ala_fields}
\alias{ala_fields}
\alias{field_info}
\title{Retrieves a list of all field names that can be used with data retrieval functions}
\usage{
ala_fields(fields_type = "occurrence")

field_info(field_id)
}
\arguments{
  \item{fields_type}{text: either "general" (for searching
  taxa, datasets, layers, and collections metadata),
  "occurrence" (for searching species occurrence records),
  or "layers" (a list of all fields associated with the
  environmental and contextual layers)}

  \item{field_id}{text: id of field for which to look up
  information. Prepend "el" for "environmental" (gridded)
  layers and "cl" for "contextual" (polygonal) layers}
}
\value{
A data frame containing the field names and various
attributes

TODO: Summary of #fields returned. We are using "layers"
here but "fields" in intersect. ids from
http://spatial.ala.org.au/ws/layers are NUMERIC but lookup
prepends "el" and "cl"! Improve error return on invalid ids
(eg "field_info("cl680"))
}
\description{
Retrieves a list of all field names that can be used with
data retrieval functions
}
\examples{
\dontrun{
o=ala_fields("occurrence")
o[1:2]
l=ala_fields("layers")
l[1,]
field_info("cl22")
field_info("el773")
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\itemize{ \item ALA web service API:
\url{http://api.ala.org.au/} \item Descriptions of the
spatial layers: \url{http://spatial.ala.org.au/layers/} }
}

