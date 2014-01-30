\name{bulklookup}
\alias{bulklookup}
\title{Bulk lookup of taxonomic names}
\usage{
bulklookup(taxa = c())
}
\arguments{
  \item{taxa}{string: a single name (string) or vector of
  names}
}
\value{
A data frame of results
}
\description{
Provides GUID, taxonomic classification, and other
information for a list of names. Fuzzy matching is used to
find the best match for each supplied name, which may be a
partial or incorrectly spelled name
}
\examples{
bulklookup(c("Grevillea humilis","Grevillea humilis subsp. maritima"))
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}
\references{
\url{http://bie.ala.org.au/bie-service/}
}

