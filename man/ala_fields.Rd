\name{ala_fields}
\alias{ala_fields}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get list of fields for searching}
\description{Retrieves a list of fields that can be used for searching}
\usage{ala_fields(fields_type = "general")}
\arguments{
  \item{fields_type}{ Either "general" (for searching taxa, datasets,
    layers, and collections metadata) or "occurrence" (for searching
    species occurrence records)
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{ Data frame containing the field names and various attributes }
\references{For "occurrence",
  \url{http://biocache.ala.org.au/ws/index/fields}. For "general", \url{http://bie.ala.org.au/admin/indexFields}
}
\author{Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
ala_fields("general")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
