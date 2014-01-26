\name{ala_occurrences}
\alias{ala_occurrences}
\title{Get occurrence data}
\description{Retrieve occurrence data via the "Get occurrences as
  gzipped CSV" web service}
\usage{
ala_occurrences(taxon = "", wkt = "", page_size = NA, fields = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxon}{Taxonomic query, e.g. "macropus"}
  \item{wkt}{A WKT (well-known text) string providing a spatial polygon
    within which to search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140 -37))"}
  \item{page_size}{Maximum number of records to return (may not be
    honoured by the ALA server). Default=NA, meaning that the server
    default value (currently 10) will be used.}
  \item{fields}{A vector of field names to return. Note that the columns
    of the returned data frame are not guaranteed to retain the ordering
    of the field names given here. See ala_fields("occurrence") for
    valid field names.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame}
\references{http://spatial.ala.org.au/layers-service/}
\author{Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
x=ala_occurrences(taxon="macropus",fields=c("longitude","latitude","common_name","taxon_name"),page_size=1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
