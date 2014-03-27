\name{autocomplete}
\alias{autocomplete}
\title{An autocomplete search for identifying species names & identifiers used at the ALA is based
on matches from a PARTIAL NAME. autocomplete can generate a dataframe of scientific and
common names that can be used for further analysis in R.}
\usage{
autocomplete(taxon, geoOnly = FALSE, idxType = NULL, limit = NULL)
}
\arguments{
  \item{taxon}{a character string of part of the
  scientific, common name of the taxa}

  \item{geoOnly}{logical: if TRUE, only results that have
  geospatial occurrence records will be included}

  \item{idxType}{string: the index type to limit. Values
  include: TAXON REGION COLLECTION INSTITUTION DATASET}

  \item{limit}{numeric: the maximum number of matches
  returned (defaults to the server-side value - currently
  10)}
}
\value{
A dataframe of taxa given the partial matches where columns
are identified as: \item{guid}{} \item{name}{}
\item{occurrenceCount}{} \item{georeferencedCount}{}
\item{scientificNameMatches}{} \item{commonNameMatches}{}
\item{commonName}{} \item{matchedNames}{} \item{ankId}{}
\item{rankString}{} \item{left}{} \item{right}{}

TODO: Suggest "search_species_autocomplete".
}
\description{
If the scientific name, common name or LSID of the taxon
are known, use fulltext_search
}
\examples{
# find information ALA holds on red kangaroo (Macropus rufus)
tt = autocomplete("red kangaroo")
tt
#show all information stored in the object
str(tt)
as.matrix(tt)

#retrieve only species with geolocations
autocomplete("Macropus rufus",geoOnly=TRUE)
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

