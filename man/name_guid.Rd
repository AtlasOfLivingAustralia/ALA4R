\name{name_guid}
\alias{name_guid}
\title{Returns the GUID for a scientific name}
\usage{
name_guid(taxon, guids_only = TRUE, verbose = ala_config()$verbose)
}
\arguments{
  \item{taxon}{string: the scientific name of interest, or
  a vector of names, i.e. c("name1","name2")}

  \item{guids_only}{logical: if TRUE, only the GUID for
  each name is returned (or an empty string if no match).
  Otherwise, additional information is also returned}
}
\value{
list: If guids_only is TRUE, a named list of character
GUIDs. Otherwise, a named list in which each element is a
list giving additional information. The accepted GUID is
the acceptedIdentifier list component
}
\description{
Returns the GUID for a scientific name
}
\examples{
name_guid("Macropus greyi")
	name_guid("Macropus greyi",guids_only=FALSE)
	name_guid(c("Macropus greyi","Pachyptila turtur","thisisnotavalidname"))
}
\author{
Ben Raymond \email{ben@theraymonds.org}, Jeremy VanDerWal
\email{jjvanderwal@gmail.com}
}
\references{
\url{http://bie.ala.org.au/bie-service/}
}

