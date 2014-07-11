\name{species_info}
\alias{species_info}
\title{Fetch a taxon profile given a scientific name or LSID (GUID)}
\usage{
species_info(scientificname, guid, verbose = ala_config()$verbose)
}
\arguments{
  \item{scientificname}{string: scientific name of the
  taxon of interest (species, genus, family etc)}

  \item{guid}{string: The Life Science Identifier of the
  taxon of interest}

  \item{verbose}{logical: show additional progress
  information? [default is set by ala_config()]}
}
\value{
species profile in the form of a named list, each element
of which is generally a data frame
}
\description{
Fetch a taxon profile given a scientific name or LSID
(GUID)
}
\examples{
s1=species_info("Grevillea humilis subsp. maritima")
str(s1)
s2=species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651")
str(s2)
s3=species_info("Alaba",verbose=TRUE)
str(s3)
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}
\seealso{
\code{\link{ala_config}}
}

