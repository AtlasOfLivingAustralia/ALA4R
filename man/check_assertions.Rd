\name{check_assertions}
\alias{check_assertions}
\title{Check assertions in occurrences object}
\usage{
check_assertions(x)
}
\arguments{
  \item{x}{list: an object returned from
  \code{\link{occurrences}}}
}
\value{
A dataframe of assertions column names, descriptions and
categories/error codes.
}
\description{
This provides a data.frame detailing the assertions that
are found in a dataset returned from
\code{\link{occurrences}}.
}
\examples{
#download species data with all possible assertions
x=occurrences(taxon="golden bowerbird",download_reason_id=10,qa=ala_fields('assertions')$name)
(asserts = check_assertions(x)) #data.frame of assertions, their description and column names
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}

