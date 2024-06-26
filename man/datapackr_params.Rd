% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packageSetup.R
\name{datapackr_params}
\alias{datapackr_params}
\title{Standardized package function parameter definitions}
\usage{
datapackr_params(
  model_data,
  model_data_path,
  snuxim_model_data,
  snuxim_model_data_path,
  undistributed_mer_data,
  SNUxIM,
  MER,
  PSNUxIM_combos,
  datapack_name,
  country_uids,
  template_path,
  submission_path,
  cached_mechs_path,
  cop_year,
  output_folder,
  results_archive,
  d2_session,
  d,
  datastreams,
  schema,
  skip,
  wb,
  PSNUs,
  psnus,
  tool,
  season,
  draft_memo,
  memo_type,
  memo_doc,
  memo_structure,
  memoStructure,
  source_type,
  prios,
  include_no_prio,
  remove_empty_columns,
  spectrum_data,
  sheet,
  sheets,
  expand_formulas,
  pzns,
  mer_data,
  ...
)
}
\arguments{
\item{model_data}{Data from DATIM needed to pack into a COP Data Pack.}

\item{model_data_path}{Local filepath to a Data Pack model data file.}

\item{snuxim_model_data}{Export from DATIM needed to allocate data across
mechanisms in the PSNUxIM tab.}

\item{snuxim_model_data_path}{Local filepath to an SNUxIM Model Data file.}

\item{undistributed_mer_data}{Data from the \code{d$datim$UndistributedMER}
dataset that can be provided while generating an OPU tool such that the
targets to be distributed will be sourced from this file.}

\item{SNUxIM}{SNUxIM dataset extract from unPackSNUxIM}

\item{MER}{MER dataset extract from unPackSheets}

\item{PSNUxIM_combos}{Dataset extract from unPackSNUxIM that shows data
missing from the PSNUxIM tab.}

\item{datapack_name}{Name you would like associated with this Data Pack.
(Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)}

\item{country_uids}{Unique IDs for countries to include in the Data Pack.
For full list of these IDs, see \code{datapackr::valid_OrgUnits}.}

\item{template_path}{Local filepath to Data Pack template Excel (XLSX) file.
This file MUST NOT have any data validation formats present. If left
\code{NULL}, will select the default based on \code{cop_year} and \code{tool}.}

\item{submission_path}{Local path to the file to import.}

\item{cached_mechs_path}{Local file path to an RDS file containing
a cached copy of the mechanisms SQL view.}

\item{cop_year}{COP Year to use for tailoring functions. Remember,
FY22 targets = COP21.}

\item{output_folder}{Local folder where you would like your Data Pack to be
saved upon export.}

\item{results_archive}{If TRUE, will export compiled results of all tests and
processes to output_folder.}

\item{d2_session}{DHIS2 Session id. R6 datimutils object which handles
authentication with DATIM.}

\item{d}{Datapackr sidecar object}

\item{datastreams}{Data stream or streams. One or more of \code{mer_targets},
\code{mer_results}, \code{subnat_targets}, \code{subnat_results}, or
\code{impatt}. If not specified, then all data streams
are returned.}

\item{schema}{Which datapackr schema to use in guiding this function. If left
\code{NULL} will select the default based on \code{cop_year} and \code{tool}.}

\item{skip}{Character vector of Sheet Names to label for skipping in schema.}

\item{wb}{Openxlsx workbook object.}

\item{PSNUs}{Dataframe of PSNUs to use in this function, containing at least
\code{psnu_uid}.}

\item{psnus}{Dataframe of PSNUs to use in this function, containing at least
\code{psnu_uid}.}

\item{tool}{Type of tool this function will create or interact with. Either
\code{OPU Data Pack} or \code{Data Pack}}

\item{season}{Either \code{COP} or \code{OPU}.}

\item{draft_memo}{Boolean indicating whether the memo being written is a
draft or final memo.}

\item{memo_type}{memo_type One of the following:
datapack: Create the memo based on the data in the datapack or OPU datapack
datim: Create the memo based on data currently in DATIM
comparison: Create a comparison memo with data from both DATIM and datapack}

\item{memo_doc}{\code{Officer} document object containing
the target memo tables.}

\item{memo_structure}{Structure of the memo d$memo$structure}

\item{memoStructure}{Structure of the memo d$memo$structure}

\item{source_type}{Indicates whether the data for a COP Approval Memo table
should come from the Data Pack or from DATIM. Values can be either
\code{datapack} or \code{datim}.}

\item{prios}{Data frame of prioritization levels.}

\item{include_no_prio}{If TRUE, include \code{"No Prioritiation"}
as a column in the output.}

\item{remove_empty_columns}{Should empty columns be removed from memos?}

\item{spectrum_data}{Spectrum output, as a dataframe.}

\item{sheet}{String. Name of sheet/tab within tool.}

\item{sheets}{Character vector. Names of sheets/tabs within tool.}

\item{expand_formulas}{Write all formulas on right side of PSNUxIM tab, not
just the first row.}

\item{pzns}{A object containing prioiritization information by organisation unit
to be added to the DataPack object.}

\item{mer_data}{If prior MER data (for instance from DATIM), is passed to this
parameter, it will be used. Otherwise, data from the existing set of tools
will be used.}

\item{...}{Additional arguments to pass.}
}
\value{
list of all paramaters of this constructor function
}
\description{
Standardized package function parameter definitions
}
\seealso{
Other parameter-helpers: 
\code{\link{parameter-checks}}
}
\concept{parameter-helpers}
