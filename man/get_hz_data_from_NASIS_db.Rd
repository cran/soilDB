% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hz_data_from_NASIS_db.R
\name{get_hz_data_from_NASIS_db}
\alias{get_hz_data_from_NASIS_db}
\title{Get Horizon Data from a local NASIS Database}
\usage{
get_hz_data_from_NASIS_db(
  SS = TRUE,
  fill = FALSE,
  stringsAsFactors = NULL,
  dsn = NULL
)
}
\arguments{
\item{SS}{fetch data from Selected Set in NASIS or from the entire local database (default: \code{TRUE})}

\item{fill}{include pedons without horizon data in result? default: \code{FALSE}}

\item{stringsAsFactors}{deprecated}

\item{dsn}{Optional: path to local SQLite database containing NASIS
table structure; default: \code{NULL}}
}
\value{
A data.frame.
}
\description{
Get horizon-level data from a local NASIS database.
}
\note{
\code{NULL} total rock fragment values are assumed to represent an \emph{absence} of rock fragments, and set to 0.
}
\seealso{
\code{\link{get_hz_data_from_NASIS_db}}, \code{\link{get_site_data_from_NASIS_db}}
}
\author{
Jay M. Skovlin and Dylan E. Beaudette
}
\keyword{manip}
