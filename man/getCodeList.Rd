% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCodeList.R
\name{getCodeList}
\alias{getCodeList}
\title{Pull & combine all MER, SUBNAT, IMPATT code lists for specified COP year.}
\usage{
getCodeList(
  cop_year = NULL,
  datastreams = c("mer_targets", "mer_results", "subnat_targets", "subnat_results",
    "impatt"),
  datasets = NULL,
  d2_session = dynGet("d2_default_session", inherits = TRUE)
)
}
\arguments{
\item{cop_year}{COP Year to use for tailoring functions. Remember,
FY22 targets = COP21.}

\item{datastreams}{Data stream or streams. One or more of \code{mer_targets},
\code{mer_results}, \code{subnat_targets}, \code{subnat_results}, or
\code{impatt}. If not specified, then all data streams
are returned.}

\item{datasets}{Character vector of dataSet IDs to pull code lists for.}

\item{d2_session}{DHIS2 Session id. R6 datimutils object which handles
authentication with DATIM.}
}
\value{
Combined code list as dataframe.
}
\description{
Pulls all code lists for MER, SUBNAT, and IMPATT for a specified COP year and
combines these into a unique list.
}
