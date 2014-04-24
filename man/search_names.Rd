\name{search_names}
\alias{search_names}
\title{Lookup of taxonomic names}
\usage{
search_names(taxa = c(), vernacular = FALSE, guids_only = FALSE)
}
\arguments{
  \item{taxa}{string: a single name or vector of names}

  \item{vernacular}{logical: if TRUE, match on common names
  as well as scientific names, otherwise match only on
  scientific names}

  \item{guids_only}{logical: if TRUE, only return a named
  list of GUIDs}
}
\value{
A data frame of results, or named list of GUIDs if
guid_only is TRUE
}
\description{
Provides GUID, taxonomic classification, and other
information for a list of names. Case-insensitive but
otherwise exact matches are used.
}
\examples{
search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima","Macropus","Thisisnot aname"))
search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima","Macropus","Thisisnot aname"),guids_only=TRUE)
search_names("Grevillea",vernacular=FALSE) ## should return the genus Grevillea
search_names("Grevillea",vernacular=TRUE) ## should return the species Grevillea banksii, because it has the common name ``Grevillea"
x=search_names("Alaba",vernacular=FALSE) ## should return info on the genus "Alaba"
str(x) ## tidy list of Alaba details
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

