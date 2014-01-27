\name{ala_autocomplete}
\alias{ala_autocomplete}
\title{Auto Complete Search}
\usage{
ala_autocomplete(taxon, limit = 10)
}
\arguments{
  \item{taxon}{a character string that defines part of the
  scientific or common name of the taxa of interest}

  \item{limit}{the maximum number of matches returned}
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
	ala_autocomplete("red kangaroo")
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}, Ben Raymond
\email{ben@theraymonds.org}
}
\references{
\url{http://www.ala.org.au/about-the-atlas/downloadable-tools/web-services/}
}

