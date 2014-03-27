\name{intersect_points}
\alias{intersect_points}
\title{Intersect environmental or contextual layers at a given a set of points (coordinates)}
\usage{
intersect_points(pnts, fids, SPdata.frame = FALSE,
  verbose = ala_config()$verbose)
}
\arguments{
  \item{points}{vector of lat & lons or 2 column data.frame
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
Confusing.
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

#Web Service: http://spatial.ala.org.au/ws/intersect/cl22,cl23/-23.1/149.1

#multiple points with multiple fields
fields = c('cl22','cl23','el773')
pnts = data.frame(lat=seq(-29,-19,0.02),lon=132.769)
intersect_points(pnts,fields)

}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}

\url{http://spatial.ala.org.au/layers/}
}

