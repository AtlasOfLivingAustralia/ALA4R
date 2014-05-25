\name{occurrences_plot}
\alias{occurrences_plot}
\title{Quick plot of occurrence data}
\usage{
occurrences_plot(x, filename = "Rplots.pdf", qa = c("fatal", "error"),
  grouped = FALSE, taxon_level = "species", ...)
}
\arguments{
  \item{x}{an list object that has been downloaded using
  \code{occurrences}}

  \item{file}{name of file to be created; defaults to
  RPlots.pdf}

  \item{qa}{string vector: list of record issues to be
  mapped; these can be assertion columnnames, or 'all' or
  'none' or any combination of 'error', 'warning' or
  'fatal'. Column or categories in your dataset can be
  viewed using \code(check_assertions).}

  \item{grouped}{logical: TRUE creates a single plot for
  all observations; FALSE plots individual maps for the
  taxon level defined.}

  \item{taxon_level}{taxanomic level at which to create
  maps; possible values include 'species', 'genus',
  'family' or 'order'}
}
\value{
Generates a pdf that maps the distributions.
}
\description{
Generates a plot of occurrence data retrieved using
\code{occurrences}.
}
\examples{
\dontrun{
#download some observations
x=occurrences(taxon="golden bowerbird",download_reason_id=10)


}
}
\author{
Atlas of Living Australia \email{support@ala.org.au}
}

