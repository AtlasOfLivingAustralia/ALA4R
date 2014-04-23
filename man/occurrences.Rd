\name{occurrences}
\alias{occurrences}
\title{Get occurrence data}
\usage{
occurrences(taxon = "", wkt = "", fields = c(),
  download_reason_id = ala_config()$download_reason_id,
  use_data_table = TRUE)
}
\arguments{
  \item{taxon}{string: taxonomic query, e.g. "Alaba vibex"}

  \item{wkt}{string: a WKT (well-known text) string
  providing a spatial polygon within which to search, e.g.
  "POLYGON((140 -37,151 -37,151 -26,140.131 -26,140 -37))"}

  \item{fields}{string vector: a vector of field names to
  return. Note that the columns of the returned data frame
  are not guaranteed to retain the ordering of the field
  names given here. See ALA4R funtion
  ala_fields("occurrence") for valid field names.}

  \item{download_reason_id}{integer: a reason code for the
  download. See ala_reasons() for valid values. The
  download_reason_id can be passed directly to this
  function, or alternatively set using
  ala_config(download_reason_id=...)}

  \item{use_data_table}{logical: if TRUE, attempt to read
  the data.csv file using the fread function from the
  data.table package. Requires data.table to be available.
  If this fails, or use_data_table is FALSE, then
  read.table will be used (which may be slow)}
}
\value{
Data frame
}
\description{
Retrieve ALA occurrence data via the "occurrence download"
web service
}
\examples{
\dontrun{
x=occurrences(taxon="macropus",fields=c("longitude","latitude","common_name","taxon_name","el807"),download_reason_id=10)
x=occurrences(taxon="data_resource_uid:dr356",download_reason_id=10)

y=occurrences(taxon="alaba vibex",fields=c("latitude","longitude","el874"),download_reason_id=10)
str(y)
qv: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Alaba\%20vibex&fields=latitude,longitude,el874&qa=none
qv: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Eucalyptus\%20gunnii&fields=latitude,longitude&qa=none&fq=basis_of_record:LivingSpecimen
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
\url{https://docs.google.com/spreadsheet/ccc?key=0AjNtzhUIIHeNdHhtcFVSM09qZ3c3N3ItUnBBc09TbHc}
\url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
}

