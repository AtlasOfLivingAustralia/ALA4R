\name{fulltext_search}
\alias{fulltext_search}
\title{Full text search}
\usage{
fulltext_search(taxon)
}
\arguments{
  \item{taxon}{string: a character string for the taxon of
  interest}
}
\value{
a named list, including the results component which is a
dataframe of information with columns being: \itemize{
\item{guid} \item{name} \item{idxtype} \item{score}
\item{parentGuid} \item{commonName} \item{nameComplete}
\item{commonNameSingle} \item{hasChildren} \item{rank}
\item{rankId} \item{rawRank} \item{conservationStatus}
\item{conservationStatusAUS} \item{isAustralian}
\item{highlight} \item{image} \item{thumbnail} \item{left}
\item{right} \item{kingdom} \item{phylum} \item{class}
\item{order} \item{class} \item{order} \item{family}
\item{genus} \item{author} \item{linkIdentifier}
\item{occCount} \item{imageSource} \item{imageCount}
\item{isEcluded} \item{imageUrl} \item{largeImageUrl}
\item{smallImageUrl} \item{thumbnailUrl}
\item{imageMetadataUrl} \item{acceptedConceptName}
\item{synonomyRelationship} \item{synonomyDescription} }
}
\description{
Performs a search across all objects, and selects the
closest matches
}
\examples{
\dontrun{
 # find information ALA holds on red kangaroo
 fulltext_search("red kangaroo")
 fulltext_search("Macropus rufus")
 fulltext_search("urn:lsid:biodiversity.org.au:afd.taxon:31a9b8b8-4e8f-4343-a15f-2ed24e0bf1ae")
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

