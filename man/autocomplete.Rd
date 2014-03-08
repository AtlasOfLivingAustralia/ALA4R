\name{autocomplete}
\alias{autocomplete}
\title{Auto Complete Search}
\usage{
autocomplete(taxon, geoOnly = FALSE, idxType = NULL, limit = NULL)
}
\arguments{
  \item{taxon}{a character string that defines part of the
  scientific or common name of the taxa of interest}

  \item{geoOnly}{logical: if TRUE, only results that have
  some geospatial occurrence records will be included}

  \item{idxType}{string: The index type to limit. Values
  include: TAXON REGION COLLECTION INSTITUTION DATASET}

  \item{limit}{the maximum number of matches returned
  (defaults to the server-side value, which is currently
  10)}
}
\value{
A dataframe of taxa given the partial matches where columns
are identified as: \item{guid}{} \item{name}{}
\item{occurrenceCount}{} \item{georeferencedCount}{}
\item{scientificNameMatches}{} \item{commonNameMatches}{}
\item{commonName}{} \item{matchedNames}{} \item{ankId}{}
\item{rankString}{} \item{left}{} \item{right}{}
}
\description{
This is an autocomplete search for identifying names &
identifiers used at ALA. It is used to provide a dataframe
of scientific and common names that can be used for further
analysis that is a match from a supplied partial name.
}
\examples{
#find information ALA holds on red kangaroo
	autocomplete("red kangaroo")
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

