% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separateDataSets.R
\name{separateDataSets}
\alias{separateDataSets}
\title{separateDataSets(d)}
\usage{
separateDataSets(data, cop_year = NULL, tool = NULL)
}
\arguments{
\item{data}{Dataframe or tibble to separate.}

\item{cop_year}{COP Year to use for tailoring functions. Remember,
FY22 targets = COP21.}

\item{tool}{Type of tool this function will create or interact with. Either
\code{OPU Data Pack} or \code{Data Pack}}
}
\value{
List object containing either MER or SUBNAT/IMPATT data.
}
\description{
After data has been extracted from all sheets in a Data Pack,
    this function separates datasets by either \code{MER} or
    \code{SUBNAT/IMPATT}.)
}
