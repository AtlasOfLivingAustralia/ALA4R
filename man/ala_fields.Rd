\name{ala_fields}
\alias{ala_fields}
\alias{field_info}
\title{Retrieve a list of all fields}
\usage{
ala_fields(fields_type = "occurrence")

field_info(field_id)
}
\arguments{
  \item{fields_type}{text: either either "general" (for
  searching taxa, datasets, layers, and collections
  metadata), "occurrence" (for searching species occurrence
  records), or "layers" (a list of all fields associated
  with the environmental and contextual layers)}

  \item{field_id}{text: id of field for which to look up
  information}
}
\value{
A data frame containing the field names and various
attributes
}
\description{
Retrieves a list of field names that can be used with the
data retrieval functions
}
\examples{
\dontrun{
ala_fields("occurrence")
field_info("cl22")
field_info("el773")
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

