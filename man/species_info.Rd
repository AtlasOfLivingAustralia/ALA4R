\name{species_info}
\alias{species_info}
\title{Fetch species profile}
\usage{
species_info(scientificname = NULL, guid = NULL,
  verbose = ala_config()$verbose)
}
\arguments{
  \item{scientificname}{string: scientific name of the
  species of interest}

  \item{guid}{string: GUID of species of interest}

  \item{verbose}{boolean value defining how much progress
  information to display; default is set by ala_config().}
}
\value{
species profile in the form of a named list
}
\description{
Retrieve species profile given either the scientific name
or GUID
}
\examples{
species_info("Grevillea humilis subsp. maritima")
species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651")
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

