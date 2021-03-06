% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deriveTargets.R
\name{deriveTargets}
\alias{deriveTargets}
\title{Derive non-Data Pack targets from others in the Data Pack}
\usage{
deriveTargets(data, type)
}
\arguments{
\item{data}{Dataframe with either Data Pack data.}

\item{type}{Type of data, either \code{Data Pack}.}
}
\value{
Dataframe with added, derived targets.
}
\description{
Takes Data Pack data and derives other targets not explicitly
set during COP.
}
