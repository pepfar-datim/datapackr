#:TODO Move these next three methods of this into the schema and make a function of the COP Year.
#This should NOT be hard coded here as it may and will change.
#' @export
#' @title Returns current COP Year
#'
#' @return Current COP Year. (e.g., for COP19, returns 2019)
#'
getCurrentCOPYear <- function() { 2021 }


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
#' @param tool "Data Pack", "Data Pack Template".
#' @param cop_year COP year for dating as well as selection of
#' templates.
#'
#' @return Character vector of tab names to skip.
#'
skip_tabs <- function(tool = "Data Pack", cop_year = getCurrentCOPYear()) {
  if (tool %in% c("Data Pack", "Data Pack Template")) {
    if (cop_year == 2020) {
      skip = c("Home", "Instructions", "Summary", "Spectrum", "Spectrum IDs")
    } else if (cop_year == 2021) {
      skip = c("Home", "Summary", "Spectrum")
    }
  }
  else if (tool == "OPU Data Pack Template" & cop_year == 2020) {
    skip = c("Home")
  } else {skip = c(NA_character_)}

  return(skip)
}

#' @export
#' @title Tool header rows
#'
#' @param tool "Data Pack", "Data Pack Template".
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Header row
#'
headerRow <- function(tool, cop_year = getCurrentCOPYear()) {

  if (cop_year %in% c(2020,2021)) {
    if (tool %in% c("Data Pack", "Data Pack Template", "OPU Data Pack Template", "OPU Data Pack")) {
      header_row <- 14
    } else stop("That tool type is not supported for that cop_year.")
  } else stop("That cop_year is not currently supported.")

  return(header_row)

}
