\name{fields}
\alias{field_info}
\alias{fields}
\title{List of fields of the environmental and contextual layers}
\usage{
fields()
}
\arguments{
  \item{field_id}{field id; the field id value from the
  field() command}
}
\value{
A data frame of results
}
\description{
Used to provide a list of all fields associated with the
environmental and contextual layers provided through ALA.
}
\examples{
fields()
field_info('cl22')
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}, Ben Raymond
\email{ben@theraymonds.org}
}
\references{
\url{http://spatial.ala.org.au/ws/}
}

