\name{bulklookup}
\alias{bulklookup}
\title{Bulk lookup of taxonomic names}
\usage{
bulklookup(taxa = c(), vernacular = FALSE)
}
\arguments{
  \item{taxa}{string: a single name or vector of names}

  \item{vernacular}{logical: if TRUE, match on common names
  as well as scientific names, otherwise match only on
  scientific names [NOT CURRENTLY IMPLEMENTED]}
}
\value{
A data frame of results
}
\description{
Provides GUID, taxonomic classification, and other
information for a list of names. Case-insensitive but
otherwise exact matches are used.
}
\examples{
bulklookup(c("Grevillea humilis","Grevillea humilis subsp. maritima","Macropus","Thisisnot aname"))
bulklookup("Grevillea",vernacular=FALSE) ## should return the genus Grevillea
bulklookup("Grevillea",vernacular=TRUE) ## should return the species Grevillea banksii, because it has the common name ``Grevillea"
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

