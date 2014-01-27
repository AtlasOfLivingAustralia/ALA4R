\name{ala_bulklookup}
\alias{ala_bulklookup}
\title{Bulk lookup of GUIDs}
\description{Used to provide GUIDs for a list of names}
\usage{ala_bulklookup(taxa = list())}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxa}{A list of names}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{A data frame of results}
\references{\url{http://bie.ala.org.au/bie-service/}}
\author{Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
ala_bulklookup(list("Grevillea humilis","Grevillea humilis subsp. maritima"))
}
