\name{check_assertions}
\alias{check_assertions}
\title{check assertions in occurrences object}
\usage{
check_assertions(x)
}
\arguments{
  \item{x}{an list object that has been downloaded using
  \code{occurrences}}
}
\value{
A dataframe of assertions column names, descriptions and
categories/error codes.
}
\description{
This provides a data.frame detailing the assertions that
are found in dataset returned from \code{occurrences}.
}
\examples{
\dontrun{
#download species data will all possible assertions
x=occurrences(taxon="golden bowerbird",download_reason_id=10,qa=ala_fields('assertions')$name)
(asserts = check_assertions(x)) #data.frame of assertions, their description and column names
}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}

