#:TODO Move these next three methods of this into the schema and make a function of the COP Year.
#This should NOT be hard coded here as it may and will change. 
#' @export
#' @title Returns current COP Year
#'
#' @return Current COP Year. (e.g., for COP19, returns 2019)
#'
getCurrentCOPYear <- function() { 2020 }


#' @export
#' @title Location of Country UIDs on Home tab.
#'
#' @return Cell reference where the name of the DataPack is located.
#'
countryUIDs_homeCell <- function() { "B25" }


#' @export
#' @title Location of Name of the DataPack on the Home tab
#'
#' @return Cell reference where the name of the datapack can be found.
#'
dataPackName_homeCell <- function() { "B20" }


#' @export
#' @title List of tabs to skip for given tool.
#'
#' @param tool "Data Pack", "Data Pack Template", "Site Tool", "Site Tool Template",
#' or "Site Filter".
#' @param cop_year COP year for dating as well as selection of
#' templates.
#'
#' @return Character vector of tab names to skip.
#'
skip_tabs <- function(tool = "Data Pack", cop_year = getCurrentCOPYear()) {
  if (tool %in% c("Data Pack", "Data Pack Template")) {
    if (cop_year == 2019) {
      skip = c("Home", "Quotes", "Summary", "Spectrum")
    }
    else if (cop_year == 2020) {
      skip = c("Home", "Instructions", "Summary", "Spectrum", "Spectrum IDs")
    }
  } else {skip = c(NA_character_)}

  return(skip)
}

#' @export
#' @title Tool header rows
#'
#' @param tool "Data Pack", "Data Pack Template", "Site Tool", "Site Tool Template",
#' or "Site Filter".
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Header row
#'
headerRow <- function(tool, cop_year = getCurrentCOPYear()) {

  if (cop_year == 2019) {
    if (tool %in% c("Data Pack","Site Tool")) {header_row <- 5}
    else if (tool %in% c("Data Pack Template","Site Tool Template")) {header_row <- 14}
    else if (tool %in% c("Site Filter")) {header_row <- 1}
    else stop("That tool type is not supported for that cop_year.")
  } else if (cop_year == 2020) {
    if (tool %in% c("Data Pack", "Data Pack Template")) {header_row <- 14}
    else stop("That tool type is not supported for that cop_year.")
  } else stop("That cop_year is not currently supported.")

  return(header_row)

}
