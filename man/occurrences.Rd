\name{occurrences}
\alias{occurrences}
\title{Retrieve point occurrence records via the "Get occurrences as gzipped CSV" web service}
\usage{
occurrences(taxon = "", wkt = "", page_size = NA, fields = c())
}
\arguments{
  \item{taxon}{Taxonomic query, e.g. "macropus rufus"}

  \item{wkt:}{A WKT (well-known text) string providing a
  spatial polygon within which to search, e.g.
  "POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140
  -37))"}

  \item{page_size}{Maximum number of records to return (may
  not be honoured by the ALA server). Default=NA, meaning
  that the server default value (currently 10) will be
  used.}

  \item{fields}{A vector of field names to return. Note
  that the columns of the returned data frame are not
  guaranteed to retain the ordering of the field names
  given here. See ala_fields("occurrence") for valid field
  names.}
}
\value{
Data frame
}
\description{
Retrieve point occurrence records via the "Get occurrences
as gzipped CSV" web service
}
\examples{
\dontrun{
x=occurrences(taxon="macropus",fields=c("longitude","latitude","common_name","taxon_name","el807"),page_size=1000)

y=occurrences(taxon="alaba vibex",fields=c("latitude","longitude"))
qv: http://biocache.ala.org.au/ws/occurrences/search?q=\%22Alaba\%20vibex\%22
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
\url{https://docs.google.com/spreadsheet/ccc?key=0AjNtzhUIIHeNdHhtcFVSM09qZ3c3N3ItUnBBc09TbHc#gid=0}
\url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
}

