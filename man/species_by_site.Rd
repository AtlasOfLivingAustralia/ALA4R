\name{species_by_site}
\alias{species_by_site}
\title{Species by site}
\usage{
species_by_site(taxon, wkt, gridsize = 0.1, SPdata.frame = FALSE,
  verbose = ala_config()$verbose)
}
\arguments{
  \item{taxon}{string: the identifier to get the species
  data from the ala biocache. E.g. "genus:Macropus".}

  \item{wkt}{string: Bounding area in Well Known Text (WKT)
  format. E.g. "POLYGON((118 -30,146 -30,146 -11,118
  -11,118 -30))".}

  \item{gridsize}{numeric: size of output grid cells in
  decimal degrees. E.g. 0.1 (=~10km)}

  \item{SPdata.frame}{logical: should the output be
  returned as a SpatialPointsDataFrame of the sp package?}

  \item{verbose}{logical: show additional progress
  information? [default is set by ala_config()]}
}
\value{
A dataframe or a SpatialPointsDataFrame containing the
species by sites data. Columns will include longitude,
latitude, and each species present. Values for species are
record counts (i.e. number of recorded occurrences of that
taxon in each grid cell)
}
\description{
A data.frame is returned as grid cells by species with
values in each cell being the number of occurrences of each
species. No null (all zero) species should be returned. The
coordinates returned are the TOP-LEFT corner of the grid
cell.
}
\examples{
\dontrun{
#download the matrix (data.frame) of Eucalyptus in Tasmania based on a 0.1 degree grid
tt=species_by_site(taxon='genus:Eucalyptus',wkt='POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))',gridsize=0.1,verbose=TRUE)
str(tt)
# equivalent direct webservice call: POST http://spatial.ala.org.au/alaspatial/ws/sitesbyspecies?speciesq=genus:Eucalyptus&qname=data&area=POLYGON((144\%20-43,148\%20-43,148\%20-40,144\%20-40,144\%20-43))&bs=http://biocache.ala.org.au/ws/&movingaveragesize=1&gridsize=0.1&sitesbyspecies=1
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/ws}

\url{http://spatial.ala.org.au/ws}

\url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
}

