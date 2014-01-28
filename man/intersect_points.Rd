\name{intersect_points}
\alias{intersect_points}
\title{Intersect a layer(s) at a given set of coordinates}
\usage{
intersect_points(pnts, fids)
}
\arguments{
  \item{pnts}{vector of lat & lons or 2 column data.frame
  or matrix of lat,lons. NOTE: the number of locations must
  be less than 1000.}

  \item{fids}{text: id of field for which to look up
  information. list of possible fields available from
  fields().}
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
#single point with multiple fields
fields = c('cl22','cl23')
pnts = c(-29,132.999)
intersect(pnts,fields)

#multiple points with multiple fields
fields = c('cl22','cl23')
pnts = data.frame(lat=seq(-29,-19,0.02),lon=132.769)
intersect(pnts,fields)
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}, Ben Raymond
\email{ben@theraymonds.org}
}
\references{
\url{http://spatial.ala.org.au/ws/}
}

