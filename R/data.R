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
#' Pack. These styles control things such as font type,font size, alignment,
#' border thickness, and color
#'
#' @format
#' \describe{
#'   \item{home}{Styles for title, Data Pack Name, & PEPFAR banner.}
#'   \item{siteList}{Styles for site names, broken out by Community, Facility,
#'   National, Military, or Inactive.}
#'   \item{data}{Styles for title, headers, labels, and Unique ID row.}
#'   \item{cop21_opu}{Styles specific to cop21_opu's}
#' }
"styleGuide"

#' @docType data
#' @title Map of COP21 indicators from Data Pack indicator codes to
#' DATIM dataelements and categoryoptioncombos
#'
#' @description
#' Dataset that maps COP21 Data Pack indicators to dataelements and
#' categoryoptioncombos in DATIM, used for mapping datasets
#' extracted from Data Packs to DATIM, with the necessary import file structure.
#'
#' @format
#' \describe{
#'   \item{indicator_code}{Code used in the Data Pack to uniquely identify each
#'   distinct programmatic area of target setting.}
#'   \item{col_type}{Values can be "target", "result" or NA}
#'   \item{value_type}{Describes what type of measure the indicator code is
#'    represented by. Values can be "integer", "percentage", or NA}
#'   \item{categoryoption_specified}{Categoryoption disaggregate of the data
#'    element}
#'   \item{valid_ages.name}{Age disaggregate}
#'   \item{valid_ages.id}{Age disaggregate UID}
#'   \item{valid_sexes.name}{Sex disaggregate}
#'   \item{valid_sexes.id}{Sex disaggregate UID}
#'   \item{valid_kps.name}{KP disaggregate}
#'   \item{valid_kps.id}{KP disaggregate UID}
#'   \item{FY}{Fiscal Year}
#'   \item{period}{DHIS2 period for example "2021Oct"}
#'   \item{categoryOptions.ids}{Categoryoption UID}
#'   \item{dataelementuid}{DATIM UID for dataElements.}
#'   \item{hts_modality}{HIV Testing service type}
#'   \item{period_dataset}{Fiscal year dataset results}
#'   \item{dataelementname}{The name of the data element being described}
#'   \item{categoryoptioncomboname}{The name of the various combinations of
#'    categories and options}
#'   \item{categoryoptioncombouid}{DATIM uid for categoryOptionCombos.}
#'   \item{targets_results}{Category variable denoting "targets" or "results"}
#'   \item{dataset}{Category variable denoting where the dateset stems from:
#'    "impatt","subnat", "mer"}
#'   \item{resultstatus}{Category variable denoting the status of the results}
#'   \item{resultststaus_inclusive}{Category variable denoting
#'    "Positive", "Negative", "Unknown"}
#'   \item{disagg_type}{Category variable denoting the dissagregate}
#'   \item{technical_area}{Category variable denoting the tecnical area}
#'   \item{top_level}{Denotes if the top level is a numerator or denominator}
#'   \item{support_type}{Category variable denoting "Sub-National", "DSD", "TA",
#'    or "No Support Type". The crossing of these with \code{indicatorCode}
#'    roughly corresponds to DATIM dataelements.}
#'   \item{numerator_denominator}{Category variable denoting numerator or
#'    denominator}
#' }
#'
"cop21_map_DataPack_DATIM_DEs_COCs"

#' @docType data
#' @title Map of COP22 indicators from Data Pack indicator codes to
#' DATIM dataelements and categoryoptioncombos
#'
#' @description
#' Dataset that maps COP22 Data Pack indicators to dataelements and
#' categoryoptioncombos in DATIM, used for mapping datasets
#' extracted from Data Packs to DATIM, with the necessary import file structure.
#'
#' @format
#' \describe{
#'   \item{indicator_code}{Code used in the Data Pack to uniquely identify each
#'   distinct programmatic area of target setting.}
#'   \item{col_type}{Values can be "target", "result" or NA}
#'   \item{value_type}{Describes what type of measure the indicator code is
#'    represented by. Values can be "integer", "percentage", or NA}
#'   \item{categoryoption_specified}{Categoryoption disaggregate of the data
#'    element}
#'   \item{valid_ages.name}{Age disaggregate}
#'   \item{valid_ages.id}{Age disaggregate UID}
#'   \item{valid_sexes.name}{Sex disaggregate}
#'   \item{valid_sexes.id}{Sex disaggregate UID}
#'   \item{valid_kps.name}{KP disaggregate}
#'   \item{valid_kps.id}{KP disaggregate UID}
#'   \item{FY}{Fiscal Year}
#'   \item{period}{DHIS2 period for example "2021Oct"}
#'   \item{categoryOptions.ids}{Categoryoption UID}
#'   \item{dataelementuid}{DATIM UID for dataElements.}
#'   \item{hts_modality}{HIV Testing service type}
#'   \item{period_dataset}{Fiscal year dataset results}
#'   \item{dataelementname}{The name of the data element being described}
#'   \item{categoryoptioncomboname}{The name of the various combinations of
#'    categories and options}
#'   \item{categoryoptioncombouid}{DATIM uid for categoryOptionCombos.}
#'   \item{targets_results}{Category variable denoting "targets" or "results"}
#'   \item{dataset}{Category variable denoting where the dateset stems from:
#'    "impatt","subnat", "mer"}
#'   \item{resultstatus}{Category variable denoting the status of the results}
#'   \item{resultststaus_inclusive}{Category variable denoting
#'    "Positive", "Negative", "Unknown"}
#'   \item{disagg_type}{Category variable denoting the dissagregate}
#'   \item{technical_area}{Category variable denoting the tecnical area}
#'   \item{top_level}{Denotes if the top level is a numerator or denominator}
#'   \item{support_type}{Category variable denoting "Sub-National", "DSD", "TA",
#'    or "No Support Type". The crossing of these with \code{indicatorCode}
#'    roughly corresponds to DATIM dataelements.}
#'   \item{numerator_denominator}{Category variable denoting numerator or
#'    denominator}
#' }
#'
"cop22_map_DataPack_DATIM_DEs_COCs"

#' @docType data
#' @title Map of indicators from Data Pack indicator codes to DATIM dataelements
#' and categoryoptioncombos
#'
#' @description
#' Dataset that maps Data Pack indicators to dataelements and
#' categoryoptioncombos in DATIM, used for mapping datasets
#' extracted from Data Packs to DATIM, with the necessary import file structure.
#'
#' @format
#' \describe{
#'   \item{indicator_code}{Code used in the Data Pack to uniquely identify each
#'   distinct programmatic area of target setting.}
#'   \item{col_type}{Values can be "target", "result" or NA}
#'   \item{value_type}{Describes what type of measure the indicator code is
#'    represented by. Values can be "integer", "percentage", or NA}
#'   \item{categoryoption_specified}{Categoryoption disaggregate of the data
#'    element}
#'   \item{valid_ages.name}{Age disaggregate}
#'   \item{valid_ages.id}{Age disaggregate UID}
#'   \item{valid_sexes.name}{Sex disaggregate}
#'   \item{valid_sexes.id}{Sex disaggregate UID}
#'   \item{valid_kps.name}{KP disaggregate}
#'   \item{valid_kps.id}{KP disaggregate UID}
#'   \item{FY}{Fiscal Year}
#'   \item{period}{DHIS2 period for example "2021Oct"}
#'   \item{categoryOptions.ids}{Categoryoption UID}
#'   \item{dataelementuid}{DATIM UID for dataElements.}
#'   \item{hts_modality}{HIV Testing service type}
#'   \item{period_dataset}{Fiscal year dataset results}
#'   \item{dataelementname}{The name of the data element being described}
#'   \item{categoryoptioncomboname}{The name of the various combinations of
#'    categories and options}
#'   \item{categoryoptioncombouid}{DATIM uid for categoryOptionCombos.}
#'   \item{targets_results}{Category variable denoting "targets" or "results"}
#'   \item{dataset}{Category variable denoting where the dateset stems from:
#'    "impatt","subnat", "mer"}
#'   \item{resultstatus}{Category variable denoting the status of the results}
#'   \item{resultststaus_inclusive}{Category variable denoting
#'    "Positive", "Negative", "Unknown"}
#'   \item{disagg_type}{Category variable denoting the dissagregate}
#'   \item{technical_area}{Category variable denoting the tecnical area}
#'   \item{top_level}{Denotes if the top level is a numerator or denominator}
#'   \item{support_type}{Category variable denoting "Sub-National", "DSD", "TA",
#'    or "No Support Type". The crossing of these with \code{indicatorCode}
#'    roughly corresponds to DATIM dataelements.}
#'   \item{numerator_denominator}{Category variable denoting numerator or
#'    denominator}
#' }
#'
"map_DataPack_DATIM_DEs_COCs"

#' @docType data
#' @title Map from Data Pack to DATIM for the adorning import files
#'
#' @description
#' Dataset that is a full map between Data Packs and DATIM for
#' the purpose of generating import and analytics tables.
#'
#' @format
#' \describe{
#'   \item{indicator_code}{Code used in the Data Pack to uniquely identify each
#'   distinct programmatic area of target setting.}
#'   \item{col_type}{Values can be "target", "result" or NA}
#'   \item{value_type}{Describes what type of measure the indicator code is
#'    represented by. Values can be "integer", "percentage", or NA}
#'   \item{categoryoption_specified}{Categoryoption disaggregate of the data
#'    element}
#'   \item{valid_ages.name}{Age disaggregate}
#'   \item{valid_ages.id}{Age disaggregate UID}
#'   \item{valid_sexes.name}{Sex disaggregate}
#'   \item{valid_sexes.id}{Sex disaggregate UID}
#'   \item{valid_kps.name}{KP disaggregate}
#'   \item{valid_kps.id}{KP disaggregate UID}
#'   \item{FY}{Fiscal Year}
#'   \item{period}{DHIS2 period for example "2021Oct"}
#'   \item{categoryOptions.ids}{Categoryoption UID}
#'   \item{dataelementuid}{DATIM UID for dataElements.}
#'   \item{hts_modality}{HIV Testing service type}
#'   \item{period_dataset}{Fiscal year dataset results}
#'   \item{dataelementname}{The name of the data element being described}
#'   \item{categoryoptioncomboname}{The name of the various combinations of
#'    categories and options}
#'   \item{categoryoptioncombouid}{DATIM uid for categoryOptionCombos.}
#'   \item{targets_results}{Category variable denoting "targets" or "results"}
#'   \item{dataset}{Category variable denoting where the dateset stems from:
#'    "impatt","subnat", "mer"}
#'   \item{resultstatus}{Category variable denoting the status of the results}
#'   \item{resultststaus_inclusive}{Category variable denoting
#'    "Positive", "Negative", "Unknown"}
#'   \item{disagg_type}{Category variable denoting the dissagregate}
#'   \item{technical_area}{Category variable denoting the tecnical area}
#'   \item{top_level}{Denotes if the top level is a numerator or denominator}
#'   \item{support_type}{Category variable denoting "Sub-National", "DSD", "TA",
#'    or "No Support Type". The crossing of these with \code{indicatorCode}
#'    roughly corresponds to DATIM dataelements.}
#'   \item{numerator_denominator}{Category variable denoting numerator or
#'    denominator}
#' }
#'
"cop22_map_adorn_import_file"

#' @docType data
#' @title List of valid PSNUs used for generating Data Packs.
#'
#' @description List of valid PSNUs used for generating Data Packs. Must be
#' synced and saved manually!
#'
#' @format
#' \describe{
#'   \item{ou}{Operating Unit name associated with the Organisation Unit}
#'   \item{ou_id}{Operating Unit UID}
#'   \item{country_name}{Country name associated with the Organisation Unit}
#'   \item{country_uid}{Country name UID}
#'   \item{snu1}{Subnational Unit associated with the Organisation Unit}
#'   \item{snu1_uid}{Subnational Unit UID}
#'   \item{psnu}{Priority Sub-National Unit associated with the Organisation
#'    Unit}
#'   \item{psnu_uid}{Priority Sub-National Unit UID}
#'   \item{psnu_type}{The type of Priority Sub-National Unit}
#'   \item{lastUpdated}{The last time the Organisation Unit was updated}
#'   \item{ancestors}{A nested eleven column data frame that contains the
#'    list of parent organisation units that contain the PSNU,
#'    including the names, ids, and which organisationUnitGroups that those
#'    parent organisation units belong to}
#'   \item{organisationUnitGroups}{A nested two column data frame that
#'    contains the name and id of the groups the organisation unit is associated
#'    with. For example "Community" and "PvuaP6YALSA"}
#'   \item{DREAMS}{Determined, Resilient, Empowered, AIDS-free, Mentored, and
#'    Safe Partnernship. Binary column "Y" or NA.}
#' }
"valid_PSNUs"

#' @docType data
#' @title List of valid COP22 PSNUs used for generating Data Packs.
#'
#' @description List of valid COP22 PSNUs used for generating Data Packs.
#' Must be synced and saved manually!
#'
#' @format
#' \describe{
#'   \item{ou}{Operating Unit name associated with the Organisation Unit}
#'   \item{ou_id}{Operating Unit UID}
#'   \item{country_name}{Country name associated with the Organisation Unit}
#'   \item{country_uid}{Country name UID}
#'   \item{snu1}{Subnational Unit associated with the Organisation Unit}
#'   \item{snu1_uid}{Subnational Unit UID}
#'   \item{psnu}{Priority Sub-National Unit associated with the Organisation
#'    Unit}
#'   \item{psnu_uid}{Priority Sub-National Unit UID}
#'   \item{psnu_type}{The type of Priority Sub-National Unit}
#'   \item{lastUpdated}{The last time the Organisation Unit was updated}
#'   \item{ancestors}{A nested eleven column data frame that contains the
#'    list of parent organisation units that contain the PSNU,
#'    including the names, ids, and which organisationUnitGroups that those
#'    parent organisation units belong to}
#'   \item{organisationUnitGroups}{A nested two column data frame that
#'    contains the name and id of the groups the organisation unit is associated
#'    with. For example "Community" and "PvuaP6YALSA"}
#'   \item{DREAMS}{Determined, Resilient, Empowered, AIDS-free, Mentored, and
#'    Safe Partnernship. Binary column "Y" or NA.}
#' }
"cop22_valid_PSNUs"


#' @docType data
#' @title Datapack country groupings
#'
#' @description Tibble of data pack country names and their UIDs.
#'
#' @format
#' \describe{
#'   \item{datapack_name}{Country name on home tab of datapack}
#'   \item{country_uids}{Country's UIDs listed on the home tab}
#' }
"COP21_datapacks_countries"


#' @docType data
#' @title Schema describing the correct structure of the COP21 OPU Data Pack
#'  template.
#'
#' @description This schema describes the correct structure of a COP21 OPU
#'  Data Pack file, generated from the template used to produce Data Packs and
#'  useful in validating Data Packs passed through datapackr.
#'
#' @format
#' \describe{
#'   \item{sheet_num}{Lists the index value associated with the sheet name
#'   listed in \code{sheet_name}.}
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack.}
#'   \item{data_structure}{Binary column describing the structure of the data
#'    These values consist of "skip" or "normal"}
#'   \item{col}{Value describing the column position of each
#'   \code{indicator_code}.}
#'   \item{indicator_code}{Code used in the Data Pack to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{dataset}{For \code{indicator_codes} listed as "\code{Targets}"
#'   in the \code{col_type} field, documents the dataset, either \code{MER},
#'   \code{IMPATT},\code{datapack}, or \code{SUBNAT}.}
#'   \item{col_type}{Flags whether an \code{indicator_code} is a Target
#'   (\code{target}), historic data (\code{past}), reference figure
#'   (\code{reference}), row header (\code{row_header}) or not (\code{NA}).}
#'   \item{value_type}{Category column describing the type of measure for the
#'    \code{indicator_code}. The values consist of "string", "integer",
#'     "percentage", or NA}
#'   \item{dataelement_dsd}{Denotes whether this element has a
#'    "Direct Service Delivery" support type}
#'   \item{dataelement_ta}{Denotes whether this element has a
#'    "Technical Assistance" support type}
#'   \item{categoryoption_specified}{Categoryoption disaggregate of the data
#'    element}
#'   \item{valid_ages}{Comprised of Age disaggregate and the associated UID}
#'   \item{valid_sexes}{Compised of Sex disaggregate and the assoicated UID}
#'   \item{valid_kps}{Compised of KP disaggregate and the assoicated UID}
#'   \item{formula}{Excel formula defined for \code{indicator_code}.}
#' }
"cop21OPU_data_pack_schema"

#' @docType data
#' @title Datapack Category option groups
#'
#' @description Data frame of category option groups (id and name)
#' along with their individual category options (id and name) as a
#' nested data frame.
#'
#' @format
#' \describe{
#'   \item{name}{Name of the Category Option Group for example "01-04 Only"}
#'   \item{id}{Category Option Group UID}
#' }
"datapack_cogs"

#' @docType data
#' @title Schema describing the correct structure of the COP21 Data Pack template.
#'
#' @description This schema describes the correct structure of a COP21 Data Pack
#' file, generated from the template used to produce Data Packs and useful in
#' validating Data Packs passed through datapackr.
#'
#' @format
#' \describe{
#'   \item{sheet_num}{Lists the index value associated with the sheet name
#'   listed in \code{sheet_name}.}
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack.}
#'   \item{data_structure}{Binary column describing the structure of the data
#'    These values consist of "skip" or "normal"}
#'   \item{col}{Value describing the column position of each
#'   \code{indicator_code}.}
#'   \item{indicator_code}{Code used in the Data Pack to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{dataset}{For \code{indicator_codes} listed as "\code{Targets}"
#'   in the \code{col_type} field, documents the dataset, either \code{MER},
#'   \code{IMPATT},\code{datapack}, or \code{SUBNAT}.}
#'   \item{col_type}{Flags whether an \code{indicator_code} is a Target
#'   (\code{target}), historic data (\code{past}), reference figure
#'   (\code{reference}), row header (\code{row_header}) or not (\code{NA}).}
#'   \item{value_type}{Category column describing the type of measure for the
#'    \code{indicator_code}. The values consist of "string", "integer",
#'     "percentage", or NA}
#'   \item{dataelement_dsd}{Denotes whether this element has a
#'    "Direct Service Delivery" support type}
#'   \item{dataelement_ta}{Denotes whether this element has a
#'    "Technical Assistance" support type}
#'   \item{categoryoption_specified}{Categoryoption disaggregate of the data
#'    element}
#'   \item{valid_ages}{Comprised of Age disaggregate and the associated UID}
#'   \item{valid_sexes}{Compised of Sex disaggregate and the assoicated UID}
#'   \item{valid_kps}{Compised of KP disaggregate and the assoicated UID}
#'   \item{formula}{Excel formula defined for \code{indicator_code}.}
#'   \item{FY}{Fiscal Year}
#'   \item{period}{DHIS2 period for example "2021Oct"}
#' }
"cop21_data_pack_schema"

#' @docType data
#' @title Schema describing the correct structure of teh COP22 Data Pack
#'  template.
#'
#' @description This schema describes the correct structure of a COP22 Data Pack
#' file, generated from the template used to produce Data Packs and useful in
#' validating Data Packs passed through datapackr.
#'
#' @format
#' \describe{
#'   \item{sheet_num}{Lists the index value associated with the sheet name
#'   listed in \code{sheet_name}.}
#'   \item{sheet_name}{Lists the sheet/tab name as used in both the Data Pack.}
#'   \item{data_structure}{Binary column describing the structure of the data
#'    These values consist of "skip" or "normal"}
#'   \item{col}{Value describing the column position of each
#'   \code{indicator_code}.}
#'   \item{indicator_code}{Code used in the Data Pack to uniquely
#'   identify each distinct programmatic area of target setting.}
#'   \item{dataset}{For \code{indicator_codes} listed as "\code{Targets}"
#'   in the \code{col_type} field, documents the dataset, either \code{MER},
#'   \code{IMPATT},\code{datapack}, or \code{SUBNAT}.}
#'   \item{col_type}{Flags whether an \code{indicator_code} is a Target
#'   (\code{target}), historic data (\code{past}), reference figure
#'   (\code{reference}), row header (\code{row_header}) or not (\code{NA}).}
#'   \item{value_type}{Category column describing the type of measure for the
#'    \code{indicator_code}. The values consist of "string", "integer",
#'     "percentage", or NA}
#'   \item{dataelement_dsd}{Denotes whether this element has a
#'    "Direct Service Delivery" support type}
#'   \item{dataelement_ta}{Denotes whether this element has a
#'    "Technical Assistance" support type}
#'   \item{categoryoption_specified}{Categoryoption disaggregate of the data
#'    element}
#'   \item{valid_ages}{Comprised of Age disaggregate and the associated UID}
#'   \item{valid_sexes}{Compised of Sex disaggregate and the assoicated UID}
#'   \item{valid_kps}{Compised of KP disaggregate and the assoicated UID}
#'   \item{formula}{Excel formula defined for \code{indicator_code}.}
#'   \item{FY}{Fiscal Year}
#'   \item{period}{DHIS2 period for example "2021Oct"}
#' }
"cop22_data_pack_schema"

#' @docType data
#' @title COP Validation Rules
#' @description A nested list of validation rules for both current and past
#' COP years.
#' @md
#'
#' @format The following COP years are included in this dataset:
#' \describe{
#'   \item{2021}{A list object containing the validation rules for COP21/FY22.}
#'   \item{2022}{A list object containing the validation rules for COP22/FY23.}
#' }
#'
#' @section Structure for COP21 data set:
#' The data set for 2021 conforms to the following structure:
#'
#' * `description`: A description of the DATIM validation rule, showing the
#' relationship required between two indicators. Synonymous to
#' the `name` and `instruction` columns.
#' * `id`: The DATIM UID for the rule.
#' * `importance`: Category showing the relative importance of the validaiton
#' rule. For COP20 and COP21, this is always listed as `MEDIUM`.
#' * `instruction`: A description of the DATIM validation rule, showing the
#' relationship required between two indicators. Synonymous to the `description`
#' and `name` columns.
#' * `name`: A description of the DATIM validation rule, showing the
#' relationship required between two indicators. Synonymous to the `description`
#' and `instruction` columns.
#' * `operator`: The operator used in the validation rule. This must be either
#' `<=`, `>=`, or `|`.
#' * `periodType`: A string indicating whether the indicator is reported
#' quartery or annually. The value is either `Quarterly` or `FinancialOct`.
#' * `ruletype`: The type of rule being applied. This value is
#' always `VALIDATION`.
#' * `leftSide.dataElements`: A nested list containing a single DATIM data
#' element defining the indicator on the left-hand side of the equation.
#' * `leftSide.description`: A description of the indicator on the left-hand
#' side of the validation rule equation.
#' * `leftSide.expression`: An expression defining how to calculate the value
#' of the left-hand side of the validation rule equation.
#' * `leftSide.missingValueStrategy`: A string that states whether this rule
#' should be skipped if the value of the left-hand side of the equation is
#' missing. Value is either `NEVER_SKIP` or `SKIP_IF_ALL_VALUES_MISSING`.
#' * `rightSide.dataElements`: A nested list containing a single DATIM data
#' element defining the indicator on the right-hand side of the equation.
#' * `rightSide.description`: A description of the indicator on the right-hand
#' side of the validation rule equation.
#' * `rightSide.expression`: An expression defining how to calculate the value
#' of the right-hand side of the validation rule equation.
#' * `rightSide.missingValueStrategy`: A string that states whether this rule
#' should be skipped if the value of the right-hand side of the equation is
#' missing. Value is either `NEVER_SKIP` or `SKIP_IF_ALL_VALUES_MISSING`.
#' * `rightSide.ops`:
#' * `leftSide.ops`:
#'
#' @section Structure for COP22 data set:
#' The data set for COP22 conforms to the following structure:
#'
#' * `name`: A descriptive name of the DATIM validation rule, showing the
#' relationship required between two indicators. Synonymous to
#' the `description` column.
#' * `id`: The DATIM UID for the rule.
#' * `periodType`: A string indicating whether the indicator is reported
#' quartery or annually. The value is either `Quarterly` or `FinancialOct`.
#' * `description`: A description of the DATIM validation rule, showing the
#' relationship required between two indicators. Synonymous to
#' the `name` column.
#' * `operator`: The operator used in the validation rule. This must be either
#' `<=`, `>=`, or `|`.
#' * `leftSide.expression`: An expression defining how to calculate the value
#' of the left-hand side of the validation rule equation.
#' * `leftSide.missingValueStrategy`:  A string that states whether this rule
#' should be skipped if the value of the left-hand side of the equation is
#' missing. Value is either `NEVER_SKIP` or `SKIP_IF_ALL_VALUES_MISSING`.
#' * `rightSide:expression`: An expression defining how to calculate the value
#' of the right-hand side of the validation rule equation.
#' * `rightSide.missingValueStrategy`:  A string that states whether this rule
#' should be skipped if the value of the right-hand side of the equation is
#' missing. Value is either `NEVER_SKIP` or `SKIP_IF_ALL_VALUES_MISSING`.
#' * `rightSide.ops`:
#' * `leftSide.ops`:
#'
#' @source \url{https://www.datim.org/}
"cop_validation_rules"
