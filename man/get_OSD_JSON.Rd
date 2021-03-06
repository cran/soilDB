% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_OSD_JSON.R
\name{get_OSD_JSON}
\alias{get_OSD_JSON}
\title{Fetch Official Series Description Raw Data in JSON format}
\usage{
get_OSD_JSON(series, base_url = NULL)
}
\arguments{
\item{series}{A character vector of Official Series names e.g. \code{"Chewacla"}}

\item{base_url}{Optional: alternate JSON repository path. Default: \code{NULL} uses \url{https://github.com/ncss-tech/SoilKnowledgeBase}.}
}
\value{
A \code{data.frame} with 1 row per series, and 1 column per "section" in the OSD as defined in National Soil Survey Handbook
}
\description{
Fetch Official Series Description Raw Data in JSON format
}
\details{
The default \code{base_url} is to "raw" JSON files stored in a GitHub repository that is regularly updated from the official source of Series Descriptions. Using format: \verb{https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/main/inst/extdata/OSD/\{LETTER\}/\{SERIES\}.json}
}
\examples{

\donttest{
if(requireNamespace("curl") &
   curl::has_internet()) {
   
series <- c("Musick", "Hector", "Chewacla")
get_OSD_JSON(series)
}
}
}
