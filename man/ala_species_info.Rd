\name{ala_species_info}
\alias{ala_species_info}
\title{Taxon or species info}
\description{Returns the profile information for a given taxon. more needed!!!}
\usage{ala_species_info(taxon)}
\arguments{
	\item{taxon}{a character string for the taxon of interest}
}
\details{
NEED
}
\value{a dataframe of information with columns being:
	\item{comp1 }{Description of 'comp1'}
}
\references{\url{http://www.ala.org.au/about-the-atlas/downloadable-tools/web-services/}}
\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}, Ben Raymond \email{ben@theraymonds.org}}
\examples{
	#find information ALA holds on red kangaroo
	ala_species_info("red kangaroo")
}
