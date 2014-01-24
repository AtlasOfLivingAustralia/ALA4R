\name{ala_specieslist}
\alias{ala_specieslist}
\title{Get list of taxa and occurrence counts}
\description{Get list of taxa and their occurrence counts, matching a
  taxonomic query and/or within a specified spatial area}
\usage{ala_specieslist(taxon = "", wkt = "", page_size = 100)}
\arguments{
  \item{taxon}{Taxonomic query, e.g. "macropus" or "macropodidae"}
  \item{wkt}{A WKT (well-known text) string providing a spatial polygon
    within which to search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140 -37))"}
  \item{page_size}{Maximum number of records to return. (May not be
    honoured by the ALA server)}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame with (at least) the columns "Scientific.name","Common.name","Taxon.rank","LSID","N.occurrences"}
\references{\url{http://spatial.ala.org.au/layers-service/}}
\author{Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
%\note{Last-Modified: <2014-01-24 19:18:12>
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
x=ala_specieslist(taxon="macropus",wkt="POLYGON((140:-37,151:-37,151:-26,140.1310:-26,140:-37))")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
