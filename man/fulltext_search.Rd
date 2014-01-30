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
Jeremy VanDerWal \email{jjvanderwal@gmail.com}, Ben Raymond
\email{ben@theraymonds.org}
}
\references{
\url{http://www.ala.org.au/about-the-atlas/downloadable-tools/web-services/}
}

