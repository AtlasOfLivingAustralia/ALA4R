\name{ala_bulklookup}
\alias{ala_bulklookup}
\title{Bulk lookup of GUIDs}
\usage{
ala_bulklookup(taxa = list())
}
\arguments{
  \item{taxa}{A list of names}
}
\value{
A data frame of results
}
\description{
Used to provide GUIDs for a list of names
}
\examples{
ala_bulklookup(list("Grevillea humilis","Grevillea humilis subsp. maritima"))
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}
\references{
\url{http://bie.ala.org.au/bie-service/}
}

