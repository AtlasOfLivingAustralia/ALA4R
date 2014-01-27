\name{enviro_layers}
\alias{enviro_layers}
\title{List of environmental and contextual layers}
\usage{
enviro_layers(type = "all")
}
\arguments{
  \item{type}{a selection of "all", "grids" or "shapes"
  associated with all possible layers, only environmental
  grids or contextual shapefiles}
}
\value{
A data frame of results
}
\description{
Used to provide a list of all environmental and contextual
layers provided through ALA.
}
\examples{
enviro_layers(type="all")
enviro_layers(type="grids")
enviro_layers(type="shapes")
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}, Ben Raymond
\email{ben@theraymonds.org}
}
\references{
\url{http://spatial.ala.org.au/ws/}
}

