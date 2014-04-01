\name{species_by_site}
\alias{species_by_site}
\title{species by site}
\usage{
species_by_site(taxon, wkt, gridsize = 0.1, SPdata.frame = FALSE,
  verbose = ala_config()$verbose)
}
\arguments{
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

TODO Lee to add dataframe output specifications TODO need
way to better check input species query
}
\description{
species by site
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

