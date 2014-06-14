\name{occurrences_s3}
\alias{occurrences_s3}
\alias{summary.occurrences}
\alias{unique.occurrences}
\title{Summarize, filter and subset occurrence data}
\usage{
summary.occurrences(object, ...)

\method{unique}{occurrences}(object, incomparables = FALSE, ...)
}
\arguments{
  \item{object}{list: a 'occurrence' object that has been
  downloaded using \code{occurrences}}

  \item{incomparables}{logical/numeric: currently ignored,
  but needed for S3 method consistency}

  \item{\dots}{not currently used}
}
\description{
Set of S3 methods to summarize, filter and get unique
occurrence data retrieved using \code{occurrences}. This
uses information based on selections of assertions (quality
assurance issues ALA has identified), spatial and temporal
data.
}
\details{
\code{unique} will give the min value for all columns that
are not used in the aggregation. \code{unique} takes a
"spatial" parameter (numeric) which specifies a rounding
value in decimal degrees used to to create a unique subset
of the data. Value of 0 means no rounding and use values as
is. Values <0 mean ignore spatial unique parameter.
}
\examples{
#download some observations
x=occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)

#summarize the occurrences
summary(x)

#keep spatially unique data at 0.01 degrees (latitude and longitude)
tt = unique(x,spatial=0.01)
summary(tt)

#remove rows that have assertions that are fatal and have spatial errors
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}

