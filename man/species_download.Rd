\name{species_download}
\alias{species_download}
\title{Download taxonomic data}
\usage{
species_download(query, fq, fields, verbose = ala_config()$verbose)
}
\arguments{
  \item{query}{string: (optional) query of the form
  field:value (e.g. "genus:Macropus") or a free text search
  ("Alaba vibex")}

  \item{fq}{string: character string or vector of strings,
  specifying filters to be applied to the original query.
  These are of the form "INDEXEDFIELD:VALUE" e.g.
  "kingdom:Fungi". See \code{ala_fields("occurrence")} for
  all the fields that are queryable. NOTE that fq matches
  are case-sensitive, but sometimes the entries in the
  fields are not consistent in terms of case (e.g. kingdom
  names "Fungi" and "Plantae" but "ANIMALIA"). fq matches
  are ANDed by default (e.g. c("field1:abc","field2:def")
  will match records that have field1 value "abc" and
  field2 value "def"). To obtain OR behaviour, use the form
  c("field1:abc OR field2:def")}

  \item{fields}{string vector: (optional) a vector of field
  names to return. Note that the columns of the returned
  data frame are not guaranteed to retain the ordering of
  the field names given here. If not specified, a default
  list of fields will be returned. See
  \code{ala_fields("general")} for valid field names.}

  \item{verbose}{logical: how much progress information to
  display; default is set by \code{ala_config()}}
}
\value{
data frame
}
\description{
Download taxonomic data
}
\examples{
# Download data for Fabaceae
x=species_download("family:Fabaceae",fields=c("guid","parentGuid","kingdom","phylum","class","order","family","genus","scientificName"))
# equivalent direct URL: http://bie.ala.org.au/ws/download?fields=guid,parentGuid,kingdom,phylum,class,order,family,genus,scientificName&q=family:Fabaceae
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}
\seealso{
\code{\link{ala_fields}}
}

