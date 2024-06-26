% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cop24_map_DataPack_DATIM_DEs_COCs}
\alias{cop24_map_DataPack_DATIM_DEs_COCs}
\title{Map of COP24 indicators from Data Pack indicator codes to
DATIM dataelements and categoryoptioncombos}
\format{
\describe{
  \item{indicator_code}{Code used in the Data Pack to uniquely identify each
  distinct programmatic area of target setting.}
  \item{col_type}{Values can be "target", "result" or NA}
  \item{value_type}{Describes what type of measure the indicator code is
   represented by. Values can be "integer", "percentage", or NA}
  \item{categoryoption_specified}{Categoryoption disaggregate of the data
   element}
  \item{valid_ages.name}{Age disaggregate}
  \item{valid_ages.id}{Age disaggregate UID}
  \item{valid_sexes.name}{Sex disaggregate}
  \item{valid_sexes.id}{Sex disaggregate UID}
  \item{valid_kps.name}{KP disaggregate}
  \item{valid_kps.id}{KP disaggregate UID}
  \item{FY}{Fiscal Year}
  \item{period}{DHIS2 period for example "2021Oct"}
  \item{categoryOptions.ids}{Categoryoption UID}
  \item{dataelementuid}{DATIM UID for dataElements.}
  \item{hts_modality}{HIV Testing service type}
  \item{period_dataset}{Fiscal year dataset results}
  \item{dataelementname}{The name of the data element being described}
  \item{categoryoptioncomboname}{The name of the various combinations of
   categories and options}
  \item{categoryoptioncombouid}{DATIM uid for categoryOptionCombos.}
  \item{targets_results}{Category variable denoting "targets" or "results"}
  \item{dataset}{Category variable denoting where the dateset stems from:
   "impatt","subnat", "mer"}
  \item{resultstatus}{Category variable denoting the status of the results}
  \item{resultststaus_inclusive}{Category variable denoting
   "Positive", "Negative", "Unknown"}
  \item{disagg_type}{Category variable denoting the dissagregate}
  \item{technical_area}{Category variable denoting the tecnical area}
  \item{top_level}{Denotes if the top level is a numerator or denominator}
  \item{support_type}{Category variable denoting "Sub-National", "DSD", "TA",
   or "No Support Type". The crossing of these with \code{indicatorCode}
   roughly corresponds to DATIM dataelements.}
  \item{numerator_denominator}{Category variable denoting numerator or
   denominator}
}
}
\usage{
cop24_map_DataPack_DATIM_DEs_COCs
}
\description{
Dataset that maps COP24 Data Pack indicators to dataelements and
categoryoptioncombos in DATIM, used for mapping datasets
extracted from Data Packs to DATIM, with the necessary import file structure.
}
\keyword{datasets}
