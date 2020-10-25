#' @docType data
#' @title Mapping of necessary Data Pack metadata
#'
#' @description A data frame containing Data Pack metadata required for executing
#' datapackr processes.
#'
#' @format A data frame with metadata for each Operating Unit's Data Pack:
#' \describe{
#'   \item{DataPack_name}{Usually the same as Operating Unit, except in rare
#'   cases for some PEPFAR Regional Operating Units.}
#'   \item{model_uid}{Usually the same as Operating Unit, except in rare cases
#'   for some PEPFAR Regional Operating Units.}
#'   \item{countryName}{Name of country as stored in DATIM organization
#'   hierarchy, even within Regional Operating Units, with the exception of
#'   _Military nodes in Regional OUs, which are necessarily listed separately
#'   as if a distinct country.}
#'   \item{countryUID}{Usually the organisationunituid as described in the DATIM
#'   organization unit hierarchy, uniquely identifying the Country or _Military
#'   node listed in \code{countryName}. For countries that are new to PEPFAR 
#'   reporting in FY20 and that were not logged in DATIM prior to the initial
#'   release of Data Packs, an arbitrary uid is assigned in this column for
#'   later re-mapping to DATIM.}
#'   \item{milPSNU}{For all countries and Regions with a _Military node, this
#'   column lists the name}
#'   \item{milPSNUuid}{For all countries and Regions with a _Military node, this
#'   column lists the DATIM organisationunituid.}
#'   \item{level3name}{Lists the \code{name} as used in level 3 of DATIM's
#'   organization unit hierarchy. For Regional OUs, this is the name of the
#'   Regional OU instead of the Country Name.}
#'   \item{uidlevel3}{Lists the \code{uid} as used in level 3 of DATIM's
#'   organization unit hierarchy. For Regional OUs, this is the uid of the
#'   Regional OU instead of the Country uid.}
#'   \item{level4name}{For countries in Regional OUs, lists the \code{name} as
#'   used in level 4 of DATIM's organization unit hierarchy. For Regional OUs,
#'   this is the name of the Country instead of the Regional OU.}
#'   \item{uidlevel4}{For countries in Regional OUs, lists the \code{uid} as
#'   used in level 4 of DATIM's organization unit hierarchy. For Regional OUs,
#'   this is the uid of the Country instead of the Regional OU.}
#'   \item{Currently_in_DATIM}{Documents whether the Country listed in 
#'   \code{countryName} was documented in DATIM's organization hierarchy as of
#'   Jan 8, 2019}
#'   \item{Military_nested_underneath}{Documents (Y/N) whether the country listed in
#'   \code{countryName} has a _Military node nested beneath it in the DATIM
#'   organization hierarchy.}
#'   \item{milPSNU_in_DATIM}{Documents (Y/N) whether nested or explicitly listed
#'   _Military node was present in DATIM as of Jan 8, 2019. Where this is a
#'   \code{N}, datapackr generates the _Military node for Data Pack use with the
#'   uid listed in \code{milPSNUuid}.}
#'   \item{Current_Prioritization_Level}{Lists the level in the DATIM
#'   organization hierarchy that contains the "Prioritization" level for each
#'   country.}
#'   \item{Prioritizing_at_Natl_or_SNU}{Documents level at which a country
#'   prioritizes for faster processing in datapackr. Options include "SNU" for 
#'   Sub-National Unit-level prioritization, "Natl" for National-level
#'   prioritization, and "Regional" for units that prioritize at Regional Level
#'   (for FY20 this is only _Military nodes in some Regional OUs).}
#'   \item{isRegion}{Logical (1/0) field documenting whether a country listed in
#'   \code{countryName} is within a Regional OU.}
#'   \item{isMil}{Logical (1/0) field documenting whether the unit listed in 
#'   \code{countryName} is a Military node.}
#' }
"configFile"

#' @docType data
#' @title Map of Data Pack dataelements and categoryoptioncombos.
#'
#' @description Maps from Data Pack indicators (which omit reference to DSD/TA
#' and are structured differently from DATIM dataelements) to DATIM dataelements
#' and categoryoptioncombos to allow for easy processing of Data Pack and Site
#' Tool data for import into and validation against DATIM.
#'
#' @format 
#' \describe{
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack.}
#'   \item{indicatorCode}{Code used in the Data Pack to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{typeOptions}{Either DSD or TA. The crossing of these with
#'   \code{indicatorCode} roughly corresponds to DATIM dataelements.}
#'   \item{dataelementuid}{DATIM uid used to uniquely describe the combination
#'   of \code{indicatorCode} and \code{typeOption}}
#'   \item{dataset}{Name of DATIM dataset associated with the listed
#'   \code{dataelementuid}, either "MER", "SUBNAT", or "IMPATT".}
#'   \item{validAges}{Age options allowable in the Data Pack for each
#'   \code{indicatorCode}}
#'   \item{validSexes}{Sex options allowable in the Data Pack for each
#'   \code{indicatorCode}}
#'   \item{validKPs}{Key Population options allowable in the Data Pack for each
#'   \code{indicatorCode}}
#'   \item{categoryoptioncombouid}{DATIM uid used to uniquely describe the
#'   combination of all disaggregates (Age, Sex, KP, etc.) for each
#'   \code{indicatorCode}}
#' }
"indicatorMap"

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
#'   \code{indicatorCode}.}
#'   \item{indicatorCode}{Code used in the Data Pack to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{colType}{Flags whether an \code{indicatorCode} is a Target
#'   (\code{"Target"}) or not (\code{NA}).}
#'   \item{dataset}{For indicatorCodes listed as "Targets" in the 
#'   \code{colType} field, documents the dataset, either "MER", "IMPATT", or
#'   "SUBNAT"}
#' }
"template_schema"

#' @docType data
#' @title Lists valid disaggs for each Data Pack tab.
#'
#' @description For each Data Pack sheet/tab, lists the Ages,
#' Sexes, and KPs allowed.
"valid_dp_disaggs"


#' @docType data
#' @title Mapping of necessary Data Pack metadata
#'
#' @description A data frame containing Data Pack metadata required for executing
#' datapackr processes.
#'
#' @format A data frame with metadata for each Operating Unit's Data Pack:
#' \describe{
#'   \item{data_pack_name}{Usually the same as Operating Unit, except in rare
#'   cases for some PEPFAR Regional Operating Units.}
#'   \item{model_uid}{Usually the same as Operating Unit, except in rare cases
#'   for some PEPFAR Regional Operating Units.}
#'   \item{country_name}{Name of country as stored in DATIM organization
#'   hierarchy}
#'   \item{country_uid}{Usually the organisationunituid as described in the DATIM
#'   organization unit hierarchy, uniquely identifying the Country listed in 
#'   \code{country_name}. For countries that are new to PEPFAR reporting in FY20
#'   and that were not logged in DATIM prior to the initial release of Data
#'   Packs, an arbitrary uid is assigned in this column for later re-mapping to
#'   DATIM.}
#'   \item{mil_psnu}{For all PEPFAR countries, this column lists the name of the
#'   _Military node.}
#'   \item{mil_psnu_uid}{For all countries with a _Military node, this column
#'   lists the DATIM organisationunituid. For _Military nodes not yet in DATIM
#'   (regional OU countries), lists "TBD".}
#'   \item{level3name}{Lists the \code{name} as used in level 3 of DATIM's
#'   organization unit hierarchy. For Regional OUs, this is the name of the
#'   Regional OU instead of the Country Name.}
#'   \item{uidlevel3}{Lists the \code{uid} as used in level 3 of DATIM's
#'   organization unit hierarchy. For Regional OUs, this is the uid of the
#'   Regional OU instead of the Country uid.}
#'   \item{level4name}{For countries in Regional OUs, lists the \code{name} as
#'   used in level 4 of DATIM's organization unit hierarchy. For Regional OUs,
#'   this is the name of the Country instead of the Regional OU.}
#'   \item{uidlevel4}{For countries in Regional OUs, lists the \code{uid} as
#'   used in level 4 of DATIM's organization unit hierarchy. For Regional OUs,
#'   this is the uid of the Country instead of the Regional OU.}
#'   \item{country_in_datim}{Documents whether the Country listed in 
#'   \code{country_name} was documented in DATIM's organization hierarchy as of
#'   Jan 8, 2019.}
#'   \item{mil_in_datim}{Documents (\code{TRUE}/\code{FALSE}) whether _Military
#'   node was present in DATIM as of Feb 20, 2019.}
#'   \item{is_region}{Logical (\code{TRUE}/\code{FALSE}) field documenting
#'   whether country in \code{country_name} is within a PEPFAR Regional OU.}
#'   \item{country}{Lists level in DATIM organization hierarchy where Country is
#'   designated.}
#'   \item{prioritization}{Lists level in DATIM organization hierarchy where
#'   prioritization occurs.}
#'   \item{planning}{Lists level in DATIM organization hierarchy where Country
#'   planning occurs.}
#'   \item{community}{Lists level in DATIM organization hierarchy where Community is
#'   designated.}
#'   \item{facility}{Lists level in DATIM organization hierarchy where Facility is
#'   designated.}
#'   \item{mil_level}{Level in DATIM organization hierarchy where _Military node
#'   is designated.}
#' }
"dataPackMap"


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
#' @title periodInfo
#' 
#' @description 
#' Dataset of current COP period info.
#' 
#' @format 
#' \describe{
#'   \item{periodid}{Unique DATIM id corresponding to period.}
#'   \item{iso}{ISO period name.}
#'   \item{startdate}{Start date of period.}
#'   \item{enddate}{End date of period.}
#'   \item{periodtype}{Period type, whether Financial, Quarterly, etc.}
#' }
#' 
"periodInfo"

#' @docType data
#' @title fake_data_flattenDataPackModel_19
#' 
#' @description 
#' Stripped down anonymized version of COP 19 model data file list object 
#' for unit testing and examples for function flattenDataPackModel_19
#' 
"fake_data_flattenDataPackModel_19"

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
#' @title Full Code List specific to current COP targets dataset
#'
#' @description Full Code List specific to current COP targets dataset,
#' generated from datim.org/api/30/sqlViews/DotdxKrNZxG.
#'
#' @format 
#' \describe{
#'   \item{dataelement}{}
#'   \item{dataelementuid}{}
#'   \item{categoryoptioncombo}{}
#'   \item{categoryoptioncombouid}{}
#' }
"fullCodeList"


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

