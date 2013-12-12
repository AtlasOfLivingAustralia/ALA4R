\name{ala_autocomplete}
\alias{ala_autocomplete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ala_autocomplete(taxon, limit = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxon}{
%%     ~~Describe \code{taxon} here~~
}
  \item{limit}{
%%     ~~Describe \code{limit} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (taxon, limit = 10) 
{
    taxon = clean_string(taxon)
    suppressWarnings({
        out = fromJSON(file = paste("http://bie.ala.org.au/ws/search/auto.json?q=", 
            taxon, "&limit=", limit, sep = ""))
    })
    tfun = function(x) {
        return(c(guid = ifelse(is.null(x$guid), NA, x$guid), 
            name = ifelse(is.null(x$name), NA, x$name), occurrenceCount = ifelse(is.null(x$occurrenceCount), 
                NA, x$occurrenceCount), georeferencedCount = ifelse(is.null(x$georeferencedCount), 
                NA, x$georeferencedCount), scientificNameMatches = ifelse(is.null(x$scientificNameMatches), 
                NA, x$scientificNameMatches), commonNameMatches = ifelse(is.null(x$commonNameMatches), 
                NA, x$commonNameMatches), commonName = ifelse(is.null(x$commonName), 
                NA, x$commonName), matchedNames = ifelse(is.null(x$matchedNames), 
                NA, x$matchedNames), rankId = ifelse(is.null(x$rankId), 
                NA, x$rankId), rankString = ifelse(is.null(x$rankString), 
                NA, x$rankString), left = ifelse(is.null(x$left), 
                NA, x$left), right = ifelse(is.null(x$right), 
                NA, x$right)))
    }
    out = do.call("rbind", lapply(out$autoCompleteList, FUN = "tfun"))
    if (is.null(out)) {
        warning("no matches found")
        return(NULL)
    }
    else {
        return(as.data.frame(out))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
