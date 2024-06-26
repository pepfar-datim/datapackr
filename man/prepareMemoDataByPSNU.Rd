% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepareMemoData.R
\name{prepareMemoDataByPSNU}
\alias{prepareMemoDataByPSNU}
\title{Prepare Memo Data By PSNU}
\usage{
prepareMemoDataByPSNU(
  analytics,
  memo_type,
  inds,
  prios,
  partners_agencies,
  psnus,
  cop_year,
  n_cores = getMaxCores()
)
}
\arguments{
\item{analytics}{Data frame consisting of at least psnu_uid,
categoryoptioncombo_id, mechanism_code and target value}

\item{memo_type}{memo_type One of the following:
datapack: Create the memo based on the data in the datapack or OPU datapack
datim: Create the memo based on data currently in DATIM
comparison: Create a comparison memo with data from both DATIM and datapack}

\item{inds}{Data frame of indicators from getMemoIndicators}

\item{prios}{Data frame of prioritization levels.}

\item{partners_agencies}{Result of getMechanismView}

\item{psnus}{Dataframe of PSNUs to use in this function, containing at least
\code{psnu_uid}.}

\item{cop_year}{COP Year to use for tailoring functions. Remember,
FY22 targets = COP21.}

\item{n_cores}{Indicates how many cores should be used.}
}
\value{
A dataframe of COP indicators aggregated to the PSNU level.
}
\description{
This function calculates COP memo indicators at the PSNU level.
If the parallel library is installed, very significant processing times can
be achieved through parallel processing.
}
