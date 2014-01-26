\name{ala_fields}
\alias{ala_fields}
\title{Retrieve a list of all fields}
\usage{
ala_fields(fields_type = "general")
}
\arguments{
  \item{fields_type}{text: either "general" (for searching
  taxa, datasets, layers, and collections metadata) or
  "occurrence" (for searching species occurrence records)}
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
ala_fields("general")
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}
\references{
For "occurrence",
\url{http://biocache.ala.org.au/ws/index/fields}.  For
"general", \url{http://bie.ala.org.au/ws/admin/indexFields}
}

