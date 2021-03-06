% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_component_data_from_NASIS_db.R
\name{get_component_data_from_NASIS_db}
\alias{get_component_data_from_NASIS_db}
\alias{get_component_restrictions_from_NASIS_db}
\title{Extract component data from a local NASIS Database}
\usage{
get_component_data_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = default.stringsAsFactors(),
  dsn = NULL
)
}
\arguments{
\item{SS}{fetch data from the currently loaded selected set in NASIS or from
the entire local database (default: \code{TRUE})}

\item{stringsAsFactors}{logical: should character vectors be converted to
factors? This argument is passed to the \code{uncode()} function. It does not
convert those vectors that have set outside of \code{uncode()} (i.e. hard coded).
The 'factory-fresh' default is TRUE, but this can be changed by setting
options(\code{stringsAsFactors = FALSE})}

\item{dsn}{Optional: path to local SQLite database containing NASIS
table structure; default: \code{NULL}}
}
\value{
A list with the results.
}
\description{
This function currently works only on Windows.
}
\examples{

\donttest{
if(local_NASIS_defined()) {
 # query text note data
 fc <- try(get_component_data_from_NASIS_db())

 # show structure of component data returned
 str(fc)
}
}

}
\seealso{
\code{\link{fetchNASIS}}
}
\author{
Dylan E. Beaudette, Stephen Roecker, and Jay M. Skovlin
}
\keyword{manip}
