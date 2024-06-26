% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{interactive_warning}
\alias{interactive_warning}
\title{Issue a warning if the session is interactive}
\usage{
interactive_warning(x)
}
\arguments{
\item{x}{Warning string.}
}
\value{
Warning message, \code{x}.
}
\description{
Supplied a string, will issue a warning as r warning() only if the session is currently interactive.
}
