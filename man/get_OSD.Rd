% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_OSD.R
\name{get_OSD}
\alias{get_OSD}
\alias{get_OSD_JSON}
\title{Get Official Series Description Data from JSON, HTML or TXT sources}
\usage{
get_OSD(
  series,
  base_url = NULL,
  result = c("json", "html", "txt"),
  fix_ocr_errors = FALSE,
  verbose = FALSE
)

get_OSD_JSON(series, base_url = NULL)
}
\arguments{
\item{series}{A character vector of Official Series names e.g. \code{"Chewacla"}}

\item{base_url}{Optional: alternate JSON/HTML/TXT repository path. Default: \code{NULL} uses \code{"https://github.com/ncss-tech/SoilKnowledgeBase"} for \code{result="json"}}

\item{result}{Select \code{"json"}, \code{"html"}, or \code{"txt"} output}

\item{fix_ocr_errors}{Default: \code{FALSE}; Applies only to \code{result='json'}. Convert clear cases of Optical Character Recognition (OCR) errors to likely actual values.}

\item{verbose}{Print errors and warning messages related to HTTP requests? Default: \code{FALSE}}
}
\value{
For JSON result: A \code{data.frame} with 1 row per series, and 1 column per "section" in the OSD as defined in National Soil Survey Handbook. For TXT or HTML result a list of character vectors containing OSD text with 1 element per series and one value per line.
}
\description{
Get Official Series Description Data from JSON, HTML or TXT sources
}
\details{
The default \code{base_url} for \code{result="json"} is to JSON files stored in a GitHub repository  that is regularly updated from the official source of Series Descriptions. Using format: \verb{https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/main/inst/extdata/OSD/\{LETTER\}/\{SERIES\}.json} for JSON. And \verb{"https://soilseriesdesc.sc.egov.usda.gov/OSD_Docs/\{LETTER\}/\{SERIES\}.html} is for \code{result="html"} (official source).

\code{fix_ocr_errors} by default is turned off (\code{FALSE}). When \code{TRUE}, assume that in color data hue/value/chroma lowercase "L" (\code{"l"}) is a 1, and a capital "O" is interpreted as zero. Also, in horizon designations assume lowercase "L" is a \code{1}, and a string that starts with \code{0} starts with the capital letter \code{"O"}.
}
\examples{
\dontshow{if (curl::has_internet() && requireNamespace("httr", quietly = TRUE) && requireNamespace("jsonlite", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontshow{\}) # examplesIf}
\donttest{
  series <- c("Musick", "Hector", "Chewacla")
  get_OSD(series)
}
}
