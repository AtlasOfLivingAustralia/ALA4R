\name{ala_cache_filename}
\alias{ala_cache_filename}
\title{Get the name of the cache file associated with the given URL}
\usage{
ala_cache_filename(url)
}
\arguments{
  \item{url}{The URL, as a string}
}
\value{
the file path and name, as a string
}
\description{
Returns the name of the cache file associated with the
given URL. Note that this file may not actually exist, this
function just provides the mapping from URL to filename
}
\examples{
ala_cache_filename("http://biocache.ala.org.au/ws/index/fields")
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}
\seealso{
\code{ala_config} for cache settings, particularly the
cache directory
}

