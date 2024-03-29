% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchPedonPC.R, R/getHzErrorsPedonPC.R
\name{fetchPedonPC}
\alias{fetchPedonPC}
\alias{getHzErrorsPedonPC}
\title{Get a SoilProfileCollection from a PedonPC v.5 database}
\usage{
fetchPedonPC(dsn)

getHzErrorsPedonPC(dsn, strict = TRUE)
}
\arguments{
\item{dsn}{The path to a PedonPC version 6.x database}

\item{strict}{Use "strict" horizon error checking? Default: \code{TRUE}}
}
\value{
a SoilProfileCollection class object
}
\description{
Fetch commonly used site/horizon data from a version 5.x PedonPC database, return as a SoilProfileCollection object.
}
\note{
This function attempts to do most of the boilerplate work when
extracting site/horizon data from a PedonPC or local NASIS database. Pedons
that have errors in their horizonation are excluded from the returned
object, however, their IDs are printed on the console. See
\code{\link{getHzErrorsPedonPC}} for a simple approach to identifying pedons
with problematic horizonation. Records from the 'taxhistory' table are
selected based on 1) most recent record, or 2) record with the least amount
of missing data.
}
\seealso{
\code{\link{get_hz_data_from_pedon_db}}
}
\author{
D. E. Beaudette and J. M. Skovlin
}
\keyword{manip}
