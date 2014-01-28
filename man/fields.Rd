\name{fields}
\alias{field_info}
\alias{fields}
\title{Retrieve a list of all fields}
\usage{
fields(fields_type = "general")

field_info(field_id)
}
\arguments{
  \item{fields_type}{text: either "general" (for searching
  taxa, datasets, layers, and collections metadata),
  "occurrence" (for searching species occurrence records),
  or "layers" (a list of all fields associated with the
  environmental and contextual layers)}

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
fields("general")
field_info('cl22')
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}
\references{
For "occurrence",
\url{http://biocache.ala.org.au/ws/index/fields}.  For
"general",
\url{http://bie.ala.org.au/ws/admin/indexFields}. For
"layers", \url{http://spatial.ala.org.au/ws/field}
}

