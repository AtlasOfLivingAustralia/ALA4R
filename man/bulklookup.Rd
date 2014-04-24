\name{bulklookup}
\alias{bulklookup}
\title{Bulk lookup of taxonomic names}
\usage{
bulklookup(taxa = c(), vernacular = FALSE, guids_only = FALSE)
}
\arguments{
  \item{taxa}{string: a single name or vector of names}

  \item{vernacular}{logical: if TRUE, match on common names
  as well as scientific names, otherwise match only on
  scientific names}

  \item{guids_only}{logical: if TRUE, only return a vector
  of GUIDs}
}
\value{
A data frame of results, or vector of GUIDs if guid_only is
TRUE
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
x=bulklookup("Alaba",vernacular=FALSE) ## should return info on the genus "Alaba"
str(x) ## tidy list of Alaba details
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

