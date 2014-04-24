\name{search_partial_name}
\alias{search_partial_name}
\title{A partial-name search for identifying species names & identifiers used at the ALA is based
on matches from a PARTIAL NAME. search_partial_name can generate a dataframe of scientific and
common names that can be used for further analysis in R.}
\usage{
search_partial_name(taxon, geo_only = FALSE, index_type, limit)
}
\arguments{
  \item{taxon}{a character string of part of the
  scientific, common name of the taxa}

  \item{geo_only}{logical: if TRUE, only results that have
  geospatial occurrence records will be included}

  \item{index_type}{string: the index type to limit. Values
  include: TAXON REGION COLLECTION INSTITUTION DATASET. By
  default, no index_type limit is applied}

  \item{limit}{numeric: the maximum number of matches
  returned (defaults to the server-side value - currently
  10)}
}
\value{
A dataframe of taxa given the partial matches where columns
are identified as: \itemize{ \item{guid}{} \item{name}{}
\item{occurrenceCount}{} \item{georeferencedCount}{}
\item{scientificNameMatches}{} \item{commonNameMatches}{}
\item{commonName}{} \item{matchedNames}{} \item{rankId}{}
\item{rankString}{} \item{left}{} \item{right}{} }
}
\description{
If searching for a taxon name, and the scientific name or
common name of the taxon are known, use search_names
instead.
}
\examples{
# find information ALA holds on red kangaroo (Macropus rufus)
tt = search_partial_name("red kangaroo")
tt
# show all information stored in the object
str(tt)
as.matrix(tt)

# retrieve only species that have geolocated occurrence records
search_partial_name("Macropus rufus",geo_only=TRUE)
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

