\name{fulltext_search}
\alias{fulltext_search}
\title{Full text search}
\usage{
fulltext_search(taxon)
}
\arguments{
  \item{taxon}{a character string for the taxon of
  interest}
}
\value{
a dataframe of information with columns being: \item{comp1
}{Description of 'comp1'}
}
\description{
Performs a search across all objects, and selects the
closest matches
}
\examples{
#find information ALA holds on red kangaroo
	fulltext_search("red kangaroo")
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

