\name{intersect_points}
\alias{intersect_points}
\title{Intersect environmental or contextual layers at a given a set of points (coordinates)}
\usage{
intersect_points(pnts, layers, SPdata.frame = FALSE, use_layer_names = TRUE,
  verbose = ala_config()$verbose)
}
\arguments{
  \item{pnts}{numeric: vector of latitude/longitude pairs,
  or a 2 column data.frame or matrix of lat,lons. NOTE: the
  number of locations must be less than 100000.}

  \item{layers}{string vector: ids of layers to be
  intersected. The list of possible layers is available
  from \code{ala_fields("layers")}. Note: if more than one
  location has been provided in \code{pnts}, the number of
  layers must be less than 700.}

  \item{SPdata.frame}{logical: should the output should be
  returned as a SpatialPointsDataFrame of the sp package or
  simply as a data.frame?}

  \item{use_layer_names}{logical: if TRUE, layer names will
  be used as column names in the returned data frame (e.g.
  "radiationLowestPeriodBio22"). Otherwise, layer id value
  will be used for column names (e.g. "el871")}

  \item{verbose}{logical: show additional progress
  information? [default is set by
  \code{\link{ala_config}}]}
}
\value{
A SpatialPointsDataFrame containing the intersecting data
information. Missing data or incorrectly identified layer
id values will result in NA data
}
\description{
Intersect environmental or contextual layers at a given a
set of points (coordinates)
}
\examples{
#single point with multiple layers
layers = c('cl22','cl23','el773')
pnts = c(-23.1,149.1)
intersect_points(pnts,layers)
# equivalent direct web service call: http://spatial.ala.org.au/ws/intersect/cl22,cl23,el773/-23.1/149.1

#multiple points as a grid sampling multiple layers
layers = c('cl22','cl23','el773')
pnts = data.frame(expand.grid(lat=seq(-29,-19,2.0),lon=seq(130.0,140.0,2.0)))
intersect_points(pnts,layers)
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}

\url{http://spatial.ala.org.au/layers/}

This function allows the user to sample
environmental/contextual layers at arbitrary locations. It
complements the \code{\link{occurrences}} function, which
allows values of the same set of layers to be downloaded at
species occurrence locations. NOTE: large requests may be
slow. A large number of points will have a bigger slowdown
than a large number of layers. Be warned. Note also that
large requests may currently fail with a "414 Request-URI
Too Large" error. This is a known limitation and under
review.
}
\seealso{
\code{\link{ala_config}}
}

