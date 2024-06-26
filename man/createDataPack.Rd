% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createDataPack.R
\name{createDataPack}
\alias{createDataPack}
\title{Create a new Data Pack}
\usage{
createDataPack(
  datapack_name = NULL,
  country_uids,
  template_path = NULL,
  cop_year = NULL,
  tool = NULL,
  d2_session = dynGet("d2_default_session", inherits = TRUE)
)
}
\arguments{
\item{datapack_name}{Name you would like associated with this Data Pack.
(Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)}

\item{country_uids}{Unique IDs for countries to include in the Data Pack.
For full list of these IDs, see \code{datapackr::valid_OrgUnits}.}

\item{template_path}{Local filepath to Data Pack template Excel (XLSX) file.
This file MUST NOT have any data validation formats present. If left
\code{NULL}, will select the default based on \code{cop_year} and \code{tool}.}

\item{cop_year}{COP Year to use for tailoring functions. Remember,
FY22 targets = COP21.}

\item{tool}{Type of tool this function will create or interact with. Either
\code{OPU Data Pack} or \code{Data Pack}}

\item{d2_session}{DHIS2 Session id. R6 datimutils object which handles
authentication with DATIM.}
}
\value{
Data Pack object
}
\description{
Creates a brand new Data Pack with the supplied characteristics.
}
\author{
Scott Jackson
}
