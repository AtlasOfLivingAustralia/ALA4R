\name{ala_cache_filename}
\alias{ala_cache_filename}
\title{Get the name of the cache file associated with the given URL}
\description{Returns the name of the cache file associated with the given
  URL. Note that this file may not actually exist, this function just provides the mapping from URL to filename }
\usage{
ala_cache_filename(url)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{The URL, as a string}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{the file path and name, as a string}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{ala_config} for cache settings, particularly the cache directory}
\examples{ala_cache_filename("http://biocache.ala.org.au/ws/index/fields")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
