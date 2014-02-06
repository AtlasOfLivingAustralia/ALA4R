\name{ala_config}
\alias{ala_config}
\alias{ala_reasons}
\title{Get or set ALA4R configuration options}
\usage{
ala_config(...)

ala_reasons()
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
download_reason_id: the ID code of the "download reason"
required by some ALA services. By default this is NA. Some
ALA services require a valid download_reason_id code,
either specified here or directly to the associated R
function. See ala_reasons() for a list of valid ID codes.
\item base_url_spatial: the base url for spatial web
services (default="http://spatial.ala.org.au/ws/") \item
base_url_bie: the base url for BIE web services
(default="http://bie.ala.org.au/ws/") \item
base_url_biocache: the base url for biocache web services
(default="http://biocache.ala.org.au/ws/") }

ala_reasons() returns a data frame with information
describing the valid options for "download_reason"
}
\examples{
ala_config()
ala_config(caching="off")
ala_reasons()
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}

