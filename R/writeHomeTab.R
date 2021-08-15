#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom utils packageVersion
#' @title writeHomeTab(wb, datapack_uid, tool = "Data Pack")
#'
#' @description
#' Function to write Home tab details into Data Pack as specified.
#'
#' @param wb Openxlsx workbook object.
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#' @param country_uids Character vector of 11 digit alphanumeric DATIM codes
#' representing countries.
#' @param cop_year COP Year in format YYYY.
#' @param tool Defaults to "Data Pack".
#'
#' @return Openxlsx workbook object with added, styled Home tab.
#'
writeHomeTab <- function(wb,
                         datapack_name,
                         country_uids,
                         cop_year = getCurrentCOPYear(),
                         tool = "Data Pack") {
  #TODO: Setup for default to run PEPFARLANDIA version.

  # Add Tab ####
  if(!any(stringr::str_detect(names(wb), "Home"))) {
    openxlsx::addWorksheet(wb,
                           sheetName = "Home",
                           gridLines = FALSE)
  }

  # PEPFAR banner ####
  openxlsx::writeData(wb, "Home", "PEPFAR", xy = c(2,2), colNames = F)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$pepfar,
                     rows = 2, cols = 2)

  # Title ####
  openxlsx::writeData(wb, "Home",
                      x = paste0("COP",
                                 stringr::str_sub(cop_year, -2,-1),
                                 " ",
                                 tool),
                      xy = c(2,10),
                      colNames = F)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$title,
                     rows = 10, cols = 2)

  # datapack_name ####
  openxlsx::writeData(wb, "Home", datapack_name, xy = c(2,20), colNames = F)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$datapack_name,
                     rows = 20, cols = 2)

  # country_uids ####
  #TODO: Can we just explicitly state row, col here?
  col <- countryUIDs_homeCell() %>%
    openxlsx::convertFromExcelRef() %>%
    as.numeric()
  row <- countryUIDs_homeCell() %>%
    stringr::str_sub(2,3) %>%
    as.numeric()

  countries <- paste(country_uids, collapse = ", ")

  openxlsx::writeData(wb, "Home", countries, xy = c(col, row), colNames = F)

  #TODO: Add feature to list country names in addition to country_uids right below

  # Generated: ####
  openxlsx::writeData(wb, "Home",
                      paste("Generated on:", Sys.time()),
                      xy = c(2, row+2),
                      colNames = F)

  # Package version ####
  openxlsx::writeData(wb, "Home",
                      paste("Package version:",
                            as.character(utils::packageVersion("datapackr"))),
                      xy = c(2, row+4))

  return(wb)
}
