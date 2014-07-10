\name{taxinfo_download}
\alias{taxinfo_download}
\title{Download taxonomic data}
\usage{
taxinfo_download(query, fq, fields, verbose = ala_config()$verbose,
  use_data_table = TRUE)
}
\arguments{
  \item{query}{string: (optional) query of the form
  field:value (e.g. "genus:Macropus") or a free text search
  ("Alaba vibex")}

  \item{fq}{string: character string or vector of strings,
  specifying filters to be applied to the original query.
  These are of the form "INDEXEDFIELD:VALUE" e.g.
  "kingdom:Fungi". See \code{ala_fields("general")} for all
  the fields that are queryable. NOTE that fq matches are
  case-sensitive, but sometimes the entries in the fields
  are not consistent in terms of case (e.g. kingdom names
  "Fungi" and "Plantae" but "ANIMALIA"). fq matches are
  ANDed by default (e.g. c("field1:abc","field2:def") will
  match records that have field1 value "abc" and field2
  value "def"). To obtain OR behaviour, use the form
  c("field1:abc OR field2:def")}

  \item{fields}{string vector: (optional) a vector of field
  names to return. Note that the columns of the returned
  data frame are not guaranteed to retain the ordering of
  the field names given here. If not specified, a default
  list of fields will be returned. See
  \code{ala_fields("general")} for valid field names}

  \item{verbose}{logical: show additional progress
  information? [default is set by ala_config()]}

  \item{use_data_table}{logical: if TRUE, attempt to read
  the data.csv file using the fread function from the
  data.table package. Requires data.table to be available.
  If this fails, or use_data_table is FALSE, then
  read.table will be used (which may be slower)}
}
\value{
data frame
}
\description{
Download taxonomic data
}
\examples{
# Download data for Fabaceae
x=taxinfo_download("family:Fabaceae",fields=c("guid","parentGuid","kingdom","phylum","class","bioOrder","family","genus","nameComplete"))
## note that requesting "nameComplete" gives the scientific name but requesting "scientificName" will not --- bug to be fixed in ALA's web service
# equivalent direct URL: http://bie.ala.org.au/ws/download?fields=guid,parentGuid,kingdom,phylum,class,bioOrder,family,genus,nameComplete&q=family:Fabaceae
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}
\seealso{
\code{\link{ala_fields}}, \code{\link{ala_config}}
}

