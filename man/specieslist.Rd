\name{specieslist}
\alias{specieslist}
\title{Get list of taxa and their occurrence counts, with/without a matching
taxonomic query within an optionally-defined WKT-defined polygon}
\usage{
specieslist(taxon = "", wkt = "", page_size = NA)
}
\arguments{
  \item{taxon:}{Text of taxon, e.g. "Macropus rufus" or
  "macropodidae"}

  \item{wkt:}{Numeric vector string of WKT (well-known
  text) defining a polygon within which to limit taxon
  search, e.g. "POLYGON((140 -37,151 -37,151 -26,140
  -26,140 -37))"}

  \item{page_size}{Maximum number of records to return (may
  not be honoured by the ALA server). Default=NA, meaning
  that the server default value (currently 10) will be
  used.}
}
\value{
Data frame with (at least) the columns \itemize{
\item{family} \item{Scientific.name} \item{Common.name}
\item{Taxon.rank} \item{GUID} \item{N.occurrences} }
}
\description{
Get list of taxa and their occurrence counts, with/without
a matching taxonomic query within an optionally-defined
WKT-defined polygon
}
\examples{
x=specieslist(taxon="macropus",wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
x=specieslist(wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
x=specieslist(wkt="POLYGON((147.62 -42.83,147.60 -42.86,147.65 -42.87,147.70 -42.86,147.62 -42.83))",page_size=30)
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
\url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
}

