% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createStaticNASIS.R
\name{createStaticNASIS}
\alias{createStaticNASIS}
\title{Create a memory or file-based instance of NASIS database}
\usage{
createStaticNASIS(
  tables = NULL,
  new_names = NULL,
  SS = TRUE,
  dsn = NULL,
  output_path = NULL,
  verbose = FALSE
)
}
\arguments{
\item{tables}{Character vector of target tables. Default: \code{NULL} is
whatever tables are listed by \code{DBI::dbListTables} for the connection typ
being used.}

\item{new_names}{Optional: new table names (should match length of vector of
matching \code{tables} in \code{dsn})}

\item{SS}{Logical. Include "selected set" tables (ending with suffix
\code{"_View_1"}). Default: \code{TRUE}}

\item{dsn}{Optional: path to SQLite database containing NASIS table
structure; or a \code{DBIConnection}. Default: \code{NULL}}

\item{output_path}{Optional: path to new/existing SQLite database to write
tables to. Default: \code{NULL} returns table results as named list.}

\item{verbose}{Show error messages from attempts to dump individual tables?
Default \code{FALSE}}
}
\value{
A named list of results from calling \code{dbQueryNASIS} for all
columns in each NASIS table.
}
\description{
Create a memory or file-based instance of NASIS database for selected tables.
}
