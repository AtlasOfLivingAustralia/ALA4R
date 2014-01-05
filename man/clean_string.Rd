\name{clean_string}
\alias{clean_string}
\title{clean up character strings}
\description{this is a simple function to remove non-ascii, alpha characters from search strings passed to ALA web services}
\usage{clean_string(x)}
\arguments{
  \item{x}{single character string}
}
\value{a cleaned character string}
\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
\examples{
	#clean up the string
	clean_string('red#    3kangaroo')
}