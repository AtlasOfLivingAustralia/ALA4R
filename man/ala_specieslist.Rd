\name{ala_specieslist}
\alias{ala_specieslist}
\title{Get list of taxa and occurrence counts}
\usage{
ala_specieslist(taxon = "", wkt = "", page_size = NA)
}
\arguments{
  \item{taxon}{Taxonomic query, e.g. "macropus" or
  "macropodidae"}

  \item{wkt}{A WKT (well-known text) string providing a
  spatial polygon within which to search, e.g.
  "POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140
  -37))"}

  \item{page_size}{Maximum number of records to return (may
  not be honoured by the ALA server). Default=NA, meaning
  that the server default value (currently 10) will be
  used.}
}
\value{
Data frame with (at least) the columns
"Scientific.name","Common.name","Taxon.rank","LSID","N.occurrences"
}
\description{
Get list of taxa and their occurrence counts, matching a
taxonomic query and/or within a specified spatial area
}
\examples{
x=ala_specieslist(taxon="macropus",wkt="POLYGON((140:-37,151:-37,151:-26,140.1310:-26,140:-37))")
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}
\references{
\url{http://spatial.ala.org.au/layers-service/}
}

