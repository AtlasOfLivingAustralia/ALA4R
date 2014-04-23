\name{intersect_points}
\alias{intersect_points}
\title{Intersect environmental or contextual layers at a given a set of points (coordinates)}
\usage{
intersect_points(pnts, fids, SPdata.frame = FALSE,
  verbose = ala_config()$verbose)
}
\arguments{
  \item{pnts}{vector of lat & lons or 2 column data.frame
  or matrix of lat,lons. NOTE: the number of locations must
  be less than 1000.}

  \item{fids}{text: ids of layers to be intersected. list
  of possible layers is available from ala_fields().}

  \item{SPdata.frame}{boolean value defining if the output
  should be returned as a SpatialPointsDataFrame of the sp
  package.}

  \item{verbose}{boolean value defining how much progress
  information to display; default is set by ala_config().}
}
\value{
A SpatialPointsDataFrame containing the intersecting data
information. Missing data or incorrectly identified field
id values will result in NA data

TODO: Pity that "fields" rather than "layers" are used.
Confusing. TODO: check that the URL strings here are
guaranteed to be appropriately URL-encoded TODO: Limit of
1000 points is a nuisance, surely?
}
\description{
Intersect environmental or contextual layers at a given a
set of points (coordinates)
}
\examples{
\dontrun{
#single point with multiple fields
fields = c('cl22','cl23','el773')
pnts = c(-23.1,149.1)
intersect_points(pnts,fields)
#Web Service equivalent: http://spatial.ala.org.au/ws/intersect/cl22,cl23,el773/-23.1/149.1

#multiple points as a grid sampling multiple fields
fields = c('cl22','cl23','el773')
pnts = data.frame(expand.grid(lat=seq(-29,-19,1.0),lon=seq(130.0,140.0,1.0)))
intersect_points(pnts,fields)

}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}

\url{http://spatial.ala.org.au/layers/}

The key related function is occurrences() where species
records can be interesected with any number of
environmental (el) and contextual (cl) layers but
intersect_points could be used to sample background (non
species-site) values of layers.
}

