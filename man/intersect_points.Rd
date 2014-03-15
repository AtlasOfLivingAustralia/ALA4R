\name{intersect_points}
\alias{intersect_points}
\title{Intersect a layer(s) at a given set of coordinates}
\usage{
intersect_points(pnts, fids, verbose = ala_config()$verbose)
}
\arguments{
  \item{pnts}{vector of lat & lons or 2 column data.frame
  or matrix of lat,lons. NOTE: the number of locations must
  be less than 1000.}

  \item{fids}{text: id of field for which to look up
  information. list of possible fields available from
  ala_fields().}

  \item{verbose}{boolean value defining how much progress
  information to display; default is set by ala_config().}
}
\value{
A data frame containing the intersecting data information.
Missing data or incorrectly identified field id values will
result in NA data
}
\description{
Intersect environmental or contextual layers given a set of
coordinates
}
\examples{
\dontrun{
#single point with multiple fields
fields = c('cl22','cl23','el773')
pnts = c(-29,132.999)
intersect_points(pnts,fields)

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
}

