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
"configFile.rda"

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
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack
#'   and Site Tool.}
#'   \item{indicatorCode}{Code used in the Data Pack and Site Tool to uniquely
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
"indicatorMap.rda"

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
"prioritizations.rda"

#' @docType data
#' @title 
#'
#' @description 
#'
#' @format 
#' \describe{
#'   \item{sheet_num}{Lists the index value associated with the sheet name
#'   listed in \code{sheet_name}.}
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack
#'   and Site Tool.}
#'   \item{col}{Value describing the column position of each 
#'   \code{indicatorCode}.)
#'   \item{indicatorCode}{Code used in the Data Pack and Site Tool to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{colType}{Flags whether an \code{indicatorCode} is a FY20 Target
#'   (\code{"FY20 Target"}) or not (\code{NA}).}
#'   \item{dataset}{For indicatorCodes listed as "FY20 Targets" in the 
#'   \code{colType} field, documents the dataset, either "MER", "IMPATT", or
#'   "SUBNAT"}
#' }
"template_schema.rda"

#' @docType data
#' @title Lists valid disaggs for each Data Pack and Site Tool tab.
#'
#' @description For each Site Tool and Data Pack sheet/tab, lists the Ages,
#' Sexes, and KPs allowed.
#' }
"valid_dp_disaggs.rds"
