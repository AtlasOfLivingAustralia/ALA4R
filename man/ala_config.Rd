\name{ala_config}
\alias{ala_config}
\title{Get or set ALA4R configuration options}
\usage{
ala_config(...)
}
\arguments{
  \item{\dots}{Options can be defined using name=value.
  Valid option names are listed above}
}
\value{
For ala_config(), a list of all options. When
ala_config(...) is called with arguments, nothing is
returned but the configuration is set.
}
\description{
Get or set configuration options that control ALA4R
behaviour
}
\details{
Invoking ala_config() with no arguments returns a list with
the current values of the options. Invoking
ala_config("reset") will reset all options to their default
values.

Valid options are: \itemize{ \item caching: caching can be
"on" (results will be cached, and any cached results will
be re-used), "refresh" (cached results will be refreshed
and the new results stored in the cache), or "off" (no
caching, default). \item cache_directory: the directory to
use for the cache. By default this is a temporary
directory, which means that results will only be cached
within an R session. The user may wish to set this to a
non-temporary directory for caching across sessions. The
directory must exist on the file system. \item verbose:
should ALA4R give verbose output to assist debugging?
(logical, default=FALSE) \item user_agent: the user-agent
string used with all web requests to the ALA servers. \item
base_url_spatial: the base url for spatial web services
(default="http://spatial.ala.org.au/ws/") \item
base_url_bie: the base url for BIE web services
(default="http://bie.ala.org.au/ws/") \item
base_url_biocache: the base url for biocache web services
(default="http://biocache.ala.org.au/ws/") }
}
\examples{
ala_config()
ala_config(caching="off")
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}

