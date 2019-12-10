#' @export
#' @title Returns current COP Year
#'
#' @return Current COP Year. (e.g., for COP19, returns 2019)
#' 
cop_year <- function() { 2019 }


#' @export
#' @title Location of Country UIDs on Home tab.
#' 
#' @return Cell reference where Country UIDs should be on Home tab.
#' 
countryUIDs_homeCell <- function() { "B25" }


#' @export
#' @title List of tabs to skip for given tool.
#' 
#' @param tool "Data Pack", "Data Pack Template", "Site Tool", "Site Tool Template",
#' "Mechanism Map", or "Site Filter".
#' @param cop_year
#' 
#' @return Character vector of tab names to skip.
#' 
skip_tabs <- function(tool = "Data Pack", cop_year = cop_year()) {
  if (tool %in% c("Data Pack", "Data Pack Template")) {
    if (cop_year == 2019) {
      skip = c("Home", "Quotes", "Summary", "Spectrum")
    }
    else if (cop_year == 2020) {
      skip = c("Home", "Summary", "Spectrum")
    }
  } else {skip = c(NA_character_)}
  
  return(skip)
}

#' @export
#' @title Tool header rows
#' 
#' @param tool "Data Pack", "Data Pack Template", "Site Tool", "Site Tool Template",
#' "Mechanism Map", or "Site Filter".
#' 
#' @return Header row
#' 
headerRow <- function(tool) {
  if (tool %in% c("Data Pack", "Site Tool")) {
    header_row <- 5
  } else if (tool %in% c("Data Pack Template", "Site Tool Template")) {
    header_row <- 14
  } else if (tool %in% c("Site Filter")) {
    header_row <- 1
  } else if (tool %in% c("Mechanism Map")) {
    header_row <- 3
  }
    
  return(header_row)
    
}