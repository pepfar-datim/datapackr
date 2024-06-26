% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilsForPDAP.R
\name{aws.executeapi}
\alias{aws.executeapi}
\title{Title}
\usage{
aws.executeapi(url, verb, query = NULL, body = "", headers = NULL)
}
\arguments{
\item{url}{URL of the PDAP Jobs API}

\item{verb}{HTTP verb to use}

\item{query}{Any query parameters which should be added to the request}

\item{body}{The body of the request. Be sure to convert to the proper format
as needed. If converting to JSON, use toJSON() from the jsonlite package.
If uploading a file, use the raw binary data.}

\item{headers}{Any additional headers which should be supplied, such as "application/csv".}
}
\value{
Returns the raw response from the API
}
\description{
Generic function to interact with the PDAP Jobs API. Based on origina
from Patrick Linton https://gist.github.com/pashri/c889ebb79c18ca77312490217b534da4
}
