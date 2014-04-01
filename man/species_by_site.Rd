\name{species_by_site}
\alias{species_by_site}
\title{Species by site}
\usage{
species_by_site(taxon, wkt, gridsize = 0.1, SPdata.frame = FALSE,
  verbose = ala_config()$verbose)
}
\arguments{
  \item{taxon}{text: the identifier to get the species data
  from the ala biocache. E.g. "genus:Macropus".}

  \item{wkt}{text: Bounding area in Well Known Text (WKT)
  format. E.g. "POLYGON((118 -30,146 -30,146 -11,118
  -11,118 -30))".}

  \item{gridsize}{numeric: size of output grid cells in
  decimal degrees. E.g. "0.1"}

  \item{SPdata.frame}{boolean value defining if the output
  should be returned as a SpatialPointsDataFrame of the sp
  package.}

  \item{verbose}{boolean value defining how much progress
  information to display; default is set by ala_config().}
}
\value{
A dataframe or a SpatialPointsDataFrame containing the
species by sites data....
}
\description{
Species by site
}
\examples{
\dontrun{
#download Macropus species information
tt = species_by_site(taxon='genus:Macropus',wkt = 'POLYGON((118 -30,146 -30,146 -11,118 -11,118 -30))',gridsize=0.1,verbose=TRUE)
head(tt)
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}

