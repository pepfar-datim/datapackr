% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchPrioritizationTable.R
\name{fetchPrioritizationTable}
\alias{fetchPrioritizationTable}
\title{Fetch Prioritization Table}
\usage{
fetchPrioritizationTable(
  psnus,
  cop_year,
  d2_session = dynGet("d2_default_session", inherits = TRUE)
)
}
\arguments{
\item{psnus}{Dataframe of PSNUs to use in this function, containing at least
\code{psnu_uid}.}

\item{cop_year}{COP Year to use for tailoring functions. Remember,
FY22 targets = COP21.}

\item{d2_session}{DHIS2 Session id. R6 datimutils object which handles
authentication with DATIM.}
}
\value{
A data frame consisting of Organisation unit, Value, prioritization
}
\description{
Fetch Prioritization Table
}
