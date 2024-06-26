% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unPackSNUxIM.R
\name{unPackSNUxIM}
\alias{unPackSNUxIM}
\title{unPackSNUxIM(d)}
\usage{
unPackSNUxIM(d)
}
\arguments{
\item{d}{Datapackr object}
}
\value{
d
}
\description{
Looks inside submitted Data Pack to extract SNU x IM data from
    \code{SNU x IM} tab and restructure this to be ready for cross-
    pollination with PSNU-level MER data coming from
    \code{\link{unPackSheets}}. This data is also analyzed to identify
    structural or data anomalies and print any issues into running Warning
    Message queue.
}
\note{
Internal functions are located in the separate unPackSNUxIM_API.R file
}
