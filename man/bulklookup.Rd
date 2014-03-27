\name{bulklookup}
\alias{bulklookup}
\title{Bulk lookup of taxonomic names: Provides GUID, taxonomic classification, and other information
for a list of species names. Fuzzy matching is used to find the best match for each supplied name,
which may be a partial or incorrectly spelled name}
\usage{
bulklookup(taxa)
}
\arguments{
  \item{taxa}{string: a single name (string) or vector of
  names}
}
\value{
A data frame of results

TODO: This is another one where some formatting of the
return would be appreciated.
}
\description{
Bulk lookup of taxonomic names: Provides GUID, taxonomic
classification, and other information for a list of species
names. Fuzzy matching is used to find the best match for
each supplied name, which may be a partial or incorrectly
spelled name
}
\examples{
\dontrun{
bulklookup(c("Grevillea humilis","Grevillea humilis subsp. maritima"))
bulklookup(c("Alaba","Eucalyptus gunnnii"))
bulklookup("cider")
bulklookup("fred")
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

