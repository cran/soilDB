% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_RMF_from_NASIS_db.R
\name{get_RMF_from_NASIS_db}
\alias{get_RMF_from_NASIS_db}
\title{Get RMF data from local NASIS}
\usage{
get_RMF_from_NASIS_db(SS = TRUE, dsn = NULL)
}
\arguments{
\item{SS}{logical, limit query to the selected set}

\item{dsn}{optional path or \emph{DBIConnection} to \link[=NASISLocalDatabase]{local database containing NASIS table structure}; default: \code{NULL}}
}
\value{
a \code{list} with two \code{data.frame} objects:
\itemize{
\item \code{RMF}: contents of "phrdxfeatures" table, often >1 row per horizon
\item \code{RMF_colors}: contents of "phredoxfcolor", usually >1 row per record in "phrdxfeatures"
}
}
\description{
Prepare a list of \code{data.frame} objects with data from the "phrdxfeatures" and "phredoxfcolor" tables. These tables are related by "phrdxfiid" column, and related to horizon data via "phiid".
}
