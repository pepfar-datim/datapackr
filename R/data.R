#' @docType data
#' @title Mapping of DATIM Prioritization numerals to strings.
#'
#' @description Maps DATIM Prioritizations from their coded numerals (1-8) to
#' their more descriptive names.
#'
#' @format
#' \describe{
#'   \item{value}{Numeral associated with DATIM Prioritization.}
#'   \item{Prioritization}{Name associated with DATIM Prioritization}
#' }
"prioritizations"

#' @docType data
#' @title Library of Openxlsx style objects to apply to both Data Pack and Site
#' Tool files.
#'
#' @description
#' A list object containing styles pertaining to different portions of the Data
#' Pack.
#'
#' @format
#' \describe{
#'   \item{home}{Styles for title, Data Pack Name, & PEPFAR banner.}
#'   \item{siteList}{Styles for site names, broken out by Community, Facility,
#'   National, Military, or Inactive.}
#'   \item{data}{Styles for title, headers, labels, and Unique ID row.}
#' }
"styleGuide"

#' @docType data
#' @title Schema describing correct structure of Data Pack template.
#'
#' @description This schema describes the correct structure of a Data Pack
#' file, generated from the template used to produce Data Packs and useful in
#' validating Data Packs passed through datapackr.
#'
#' @format
#' \describe{
#'   \item{sheet_num}{Lists the index value associated with the sheet name
#'   listed in \code{sheet_name}.}
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack.}
#'   \item{col}{Value describing the column position of each
#'   \code{indicator_code}.}
#'   \item{label}{String label used to describe \code{indicator_code}.}
#'   \item{indicator_code}{Code used in the Data Pack to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{formula}{Excel formula defined for \code{indicator_code}.}
#'   \item{col_type}{Flags whether an \code{indicator_code} is a Target
#'   (\code{"Target"}), row header (\code{Row Header}) or not (\code{NA}).}
#'   \item{dataset}{For \code{indicator_codes} listed as "\code{Targets}"
#'   in the \code{col_type} field, documents the dataset, either \code{MER},
#'   \code{IMPATT}, or \code{SUBNAT}.}
#' }
"data_pack_schema"

#' @docType data
#' @title Map of indicators from Data Pack indicator codes to DATIM dataelements
#' and categoryoptioncombos
#'
#' @description
#' Dataset that maps Data Pack indicators to dataelements and
#' categoryoptioncombos in DATIM, used for mapping datasets
#' extracted from Data Packs to DATIM import file structure.
#'
#' @format
#' \describe{
#'   \item{indicator_code}{Code used in the Data Pack to uniquely identify each
#'   distinct programmatic area of target setting.}
#'   \item{categoryoption_specified}{}
#'   \item{valid_ages.name}{Age disaggregate}
#'   \item{valid_ages.id}{Age disaggregate UID}
#'   \item{valid_sexes.name}{Sex disaggregate}
#'   \item{valid_sexes.id}{Sex disaggregate UID}
#'   \item{valid_kps.name}{KP disaggregate}
#'   \item{valid_kps.id}{KP disaggregate UID}
#'   \item{categoryOptions.ids}{}
#'   \item{support_type}{Either DSD or TA. The crossing of these with
#'   \code{indicatorCode} roughly corresponds to DATIM dataelements.}
#'   \item{dataelement}{DATIM uid for dataElements.}
#'   \item{hts_modality}{}
#'   \item{tech_area}{}
#'   \item{numerator_denominator}{}
#'   \item{dataelement.y}{}
#'   \item{categoryoptioncombo}{}
#'   \item{categoryoptioncombouid}{DATIM uid for categoryOptionCombos.}
#'   \item{resultstatus}{}
#'   \item{resultststaus_inclusive}{}
#'   \item{disagg_type}{}
#'   \item{technical_area}{}
#'   \item{top_level}{}
#' }
#'
"map_DataPack_DATIM_DEs_COCs"


#' @docType data
#' @title Schema describing correct structure of COP20 Data Pack template.
#'
#' @description This schema describes the correct structure of a COP20 Data Pack
#' file, generated from the template used to produce Data Packs and useful in
#' validating Data Packs passed through datapackr.
#'
#' @format
#' \describe{
#'   \item{sheet_num}{Lists the index value associated with the sheet name
#'   listed in \code{sheet_name}.}
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack.}
#'   \item{data_structure}{}
#'   \item{col}{Value describing the column position of each
#'   \code{indicator_code}.}
#'   \item{indicator_code}{Code used in the Data Pack to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{dataset}{For \code{indicator_codes} listed as "\code{Targets}"
#'   in the \code{col_type} field, documents the dataset, either \code{MER},
#'   \code{IMPATT}, or \code{SUBNAT}.}
#'   \item{col_type}{Flags whether an \code{indicator_code} is a Target
#'   (\code{"Target"}), row header (\code{Row Header}) or not (\code{NA}).}
#'   \item{value_type}{}
#'   \item{dataelement_dsd}{}
#'   \item{dataelement_ta}{}
#'   \item{categoryoption_specified}{}
#'   \item{valid_ages}{}
#'   \item{valid_sexes}{}
#'   \item{valid_kps}{}
#'   \item{formula}{Excel formula defined for \code{indicator_code}.}
#' }
"cop20_data_pack_schema"

#' @docType data
#' @title List of valid PSNUs used for generating Data Packs.
#'
#' @description List of valid PSNUs used for generating Data Packs. Must be
#' synced and saved manually.
#'
#' @format
#' \describe{
#'   \item{ou}{}
#'   \item{ou_id}{}
#'   \item{country_name}{}
#'   \item{country_uid}{}
#'   \item{snu1}{}
#'   \item{snu1_uid}{}
#'   \item{psnu}{}
#'   \item{psnu_uid}{}
#'   \item{psnu_type}{}
#'   \item{lastUpdated}{}
#'   \item{ancestors}{}
#'   \item{organisationUnitGroups}{}
#' }
"valid_PSNUs"

#' @docType data
#' @title List of  valid categoryoptions based on current COP Target Code List.
#'
#' @description List of valid categoryoptions based on current COP Target Code
#' List. Must be synced and saved manually.
#'
#' @format
#' \describe{
#'   \item{name}{}
#'   \item{id}{}
#'   \item{categoryoptiongroup}{}
#'   \item{datapack_disagg}{}
#'   \item{datapack_schema_group}{}
#' }
"valid_category_options"

#' @docType data
#' @title Datapack country groupings
#'
#' @description Tibble of data pack names and
#'
#' @format
#' \describe{
#'   \item{datapack_name}{Name on home tab of datapack}
#'   \item{country_uids}{countries uids listed on home tab}
#' }
"COP21_datapacks_countries"
