\name{search_fulltext}
\alias{search_fulltext}
\title{Full text search}
\usage{
search_fulltext(taxon, fq, start, page_size, sort_by, sort_dir)
}
\arguments{
  \item{taxon}{string: (required) a character string for
  the taxon of interest}

  \item{fq}{string: (optional) character string or vector
  of strings, specifying filters to be applied to the
  original query. These are of the form
  "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". See
  ala_fields("general") for all the fields that are
  queryable. NOTE that fq matches are case-sensitive, but
  sometimes the entries in the fields are not consistent in
  terms of case (e.g. kingdom names "Fungi" and "Plantae"
  but "ANIMALIA"). fq matches are ANDed by default (e.g.
  c("field1:abc","field2:def") will match records that have
  field1 value "abc" and field2 value "def"). To obtain OR
  behaviour, use the form c("field1:abc OR field2:def")}

  \item{start}{numeric: (optional) (positive integer) start
  offset for the results}

  \item{page_size}{numeric: (optional) (positive integer)
  maximum number of records to return. Defaults to the
  server-side value - currently 10}

  \item{sort_by}{string: (optional) field to sort on}

  \item{sort_dir}{string: (optional) sort direction, either
  "asc" or "desc"}
}
\value{
a named list with the components "meta" (search metadata),
"facets" (search facet results), and "data" (the search
results, in the form of a dataframe).
}
\description{
Performs a search across all objects, and selects the
closest matches
}
\examples{
# find information ALA holds on red kangaroo
 search_fulltext("red kangaroo")
 search_fulltext("Macropus rufus")
 search_fulltext("urn:lsid:biodiversity.org.au:afd.taxon:31a9b8b8-4e8f-4343-a15f-2ed24e0bf1ae")

 # find genus names like "Oenanthe"
 search_fulltext("oenanthe",sort_by="kingdom",fq="rank:genus")
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}
\references{
\url{http://api.ala.org.au/}
}

