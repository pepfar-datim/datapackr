% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unPackPSNUxIM_API.R
\name{calculateFinalDedupeValues}
\alias{calculateFinalDedupeValues}
\title{Title calculateFinalDedupeValues}
\usage{
calculateFinalDedupeValues(d, header_cols)
}
\arguments{
\item{d}{Datapackr d object}

\item{header_cols}{header_cols object}
}
\value{
d object with final dedupe values
}
\description{
Imputes zeros for dedupe values (if needed), keeps valid
depudes (where valid) and removes any dedupes which should not exist
based on the logic of pure and crosswalk deduplication.
}
