\name{species_info}
\alias{species_info}
\title{Fetch a taxon profile given a scientific name or LSID}
\usage{
species_info(scientificname = NULL, guid = NULL,
  verbose = ala_config()$verbose)
}
\arguments{
  \item{scientificname}{string: scientific name of the
  taxon of interest (species, genus, family etc)}

  \item{guid}{string: The Life Science Identifier of the
  taxon of interest}

  \item{verbose}{boolean value: How much progress
  information to display; default is set by ala_config().}
}
\value{
species profile in the form of a named list
}
\description{
Fetch a taxon profile given a scientific name or LSID
}
\examples{
\dontrun{
species_info("Grevillea humilis subsp. maritima")
species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651")
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

