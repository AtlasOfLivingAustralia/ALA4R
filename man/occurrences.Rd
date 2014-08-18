\name{occurrences}
\alias{occurrences}
\title{Get occurrence data}
\usage{
occurrences(taxon, wkt, fq, fields, extra, qa,
  download_reason_id = ala_config()$download_reason_id, reason,
  verbose = ala_config()$verbose, record_count_only = FALSE,
  use_layer_names = TRUE, use_data_table = TRUE)
}
\arguments{
  \item{taxon}{string: (optional) taxonomic query of the
  form field:value (e.g. "genus:Macropus") or a free text
  search ("Alaba vibex")}

  \item{wkt}{string: (optional) a WKT (well-known text)
  string providing a spatial polygon within which to
  search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.131
  -26,140 -37))"}

  \item{fq}{string: (optional) character string or vector
  of strings, specifying filters to be applied to the
  original query. These are of the form
  "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". See
  \code{ala_fields("occurrence")} for all the fields that
  are queryable. NOTE that fq matches are case-sensitive,
  but sometimes the entries in the fields are not
  consistent in terms of case (e.g. kingdom names "Fungi"
  and "Plantae" but "ANIMALIA"). fq matches are ANDed by
  default (e.g. c("field1:abc","field2:def") will match
  records that have field1 value "abc" and field2 value
  "def"). To obtain OR behaviour, use the form
  c("field1:abc OR field2:def")}

  \item{fields}{string vector: (optional) a vector of field
  names to return. Note that the columns of the returned
  data frame are not guaranteed to retain the ordering of
  the field names given here. If not specified, a default
  list of fields will be returned. See
  \code{ala_fields("occurrence")} for valid field names.
  Field names can be passed as full names (e.g. "Radiation
  - lowest period (Bio22)") rather than id ("el871")}

  \item{extra}{string vector: (optional) a vector of field
  names to include in addition to those specified in
  \code{fields}. This is useful if you would like the
  default list of fields (i.e. when \code{fields} parameter
  is not specified) plus some additional extras. See
  \code{ala_fields("occurrence")} for valid field names.
  Field names can be passed as full names (e.g. "Radiation
  - lowest period (Bio22)") rather than id ("el871")}

  \item{qa}{string vector: (optional) list of record issues
  to include in the download. See
  \code{ala_fields("assertions")} for valid values, or use
  "none" to include no record issues}

  \item{download_reason_id}{numeric or string: (required
  unless record_count_only is TRUE) a reason code for the
  download, either as a numeric ID (currently 0--11) or a
  string (see \code{\link{ala_reasons}} for a list of valid
  ID codes and names). The download_reason_id can be passed
  directly to this function, or alternatively set using
  \code{ala_config(download_reason_id=...)}}

  \item{reason}{string: (optional) user-supplied
  description of the reason for the download. Providing
  this information is optional but will help the ALA to
  better support users by building a better understanding
  of user communities and their data requests}

  \item{verbose}{logical: show additional progress
  information? [default is set by ala_config()]}

  \item{record_count_only}{logical: if TRUE, return just
  the count of records that would be downloaded, but don't
  download them. Note that the record count is always
  re-retrieved from the ALA, regardless of the caching
  settings. If a cached copy of this query exists on the
  local machine, the actual data set size may therefore
  differ from this record count}

  \item{use_layer_names}{logical: if TRUE, layer names will
  be used as layer column names in the returned data frame
  (e.g. "radiationLowestPeriodBio22"). Otherwise, layer id
  value will be used for layer column names (e.g. "el871")}

  \item{use_data_table}{logical: if TRUE, attempt to read
  the data.csv file using the fread function from the
  data.table package. Requires data.table to be available.
  If this fails with an error or warning, or if
  use_data_table is FALSE, then read.table will be used
  (which may be slower)}
}
\value{
Data frame of occurrence results, with one row per
occurrence record. The columns of the dataframe will depend
on the requested fields
}
\description{
Retrieve ALA occurrence data via the "occurrence download"
web service. At least one of \code{taxon}, \code{wkt}, or
\code{fq} must be supplied for a valid query. Note that the
current service is limited to a maximum of 500000 records
per request.
}
\examples{
x=occurrences(taxon="data_resource_uid:dr356",record_count_only=TRUE) ## count of records from this data provider
x=occurrences(taxon="data_resource_uid:dr356",download_reason_id=10) ## download records, with standard fields
\dontrun{
x=occurrences(taxon="data_resource_uid:dr356",download_reason_id=10,fields=ala_fields("occurrence")$name) ## download records, with all fields
x=occurrences(taxon="macropus",fields=c("longitude","latitude","common_name","taxon_name","el807"),download_reason_id=10) ## download records, with specified fields
x=occurrences(taxon="macropus",wkt="POLYGON((145 -37,150 -37,150 -30,145 -30,145 -37))",download_reason_id=10,qa="none") ## download records in polygon, with no quality assertion information

y=occurrences(taxon="alaba vibex",fields=c("latitude","longitude","el874"),download_reason_id=10)
str(y)
# equivalent direct webservice call: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Alaba\%20vibex&fields=latitude,longitude,el874&qa=none

occurrences(taxon="Eucalyptus gunnii",fields=c("latitude","longitude"),qa="none",fq="basis_of_record:LivingSpecimen",download_reason_id=10)
# equivalent direct webservice call: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Eucalyptus\%20gunnii&fields=latitude,longitude&qa=none&fq=basis_of_record:LivingSpecimen
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\itemize{ \item \url{http://api.ala.org.au/} \item Field
definitions:
\url{https://docs.google.com/spreadsheet/ccc?key=0AjNtzhUIIHeNdHhtcFVSM09qZ3c3N3ItUnBBc09TbHc}
\item WKT reference:
\url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
}
}
\seealso{
\code{\link{ala_reasons}} for download reasons;
\code{\link{ala_config}}
}

