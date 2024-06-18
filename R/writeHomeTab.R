#' @export
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
writeHomeTab <- function(wb = NULL,
                         datapack_name = NULL,
                         country_uids,
                         cop_year = getCurrentCOPYear(),
                         tool = "Data Pack") {

  # Check & assign params
  params <- check_params(
    country_uids = country_uids,
    datapack_name = datapack_name,
    cop_year = cop_year,
    tool = tool,
    wb = wb)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  # Add Tab ####
  if (!any(stringr::str_detect(names(wb), "Home"))) {
    openxlsx::addWorksheet(wb,
                           sheetName = "Home",
                           gridLines = FALSE)
  }

  # PEPFAR banner ####
  openxlsx::writeData(wb, "Home", "PEPFAR", xy = c(2, 2), colNames = FALSE)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$pepfar,
                     rows = 2, cols = 2)

  # Title ####
  if (cop_year %in% c(2023, 2024,2025) && tool == "Data Pack") {
    tool_title <- "Target Setting Tool"
  } else {
    tool_title <- tool
  }

  pd <- "COP"
  yr <- cop_year - 2000
  if (cop_year %in% c(2024,2025)) {
    pd <- "FY"
    yr <- yr + 1
  }

  openxlsx::writeData(wb, "Home",
                      x = paste0(pd, yr, " ", tool_title),
                      xy = c(2, 10),
                      colNames = FALSE)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$title,
                     rows = 10, cols = 2)

  # datapack_name ####
  openxlsx::writeData(wb, "Home", datapack_name, xy = c(2, 20), colNames = FALSE)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$datapack_name,
                     rows = 20, cols = 2)

  # country_uids ####
  col <- countryUIDs_homeCell() %>%
    openxlsx::convertFromExcelRef() %>%
    as.numeric()
  row <- countryUIDs_homeCell() %>%
    stringr::str_sub(2, 3) %>%
    as.numeric()

  countries <- paste(country_uids, collapse = ", ")

  openxlsx::writeData(wb, "Home", countries, xy = c(col, row), colNames = FALSE)

  # country_names ####
  country_names <-
    check_params(country_uids = country_uids, datapack_name = NULL, cop_year = cop_year) %>%
    purrr::pluck("datapack_name")

  openxlsx::writeData(wb,
                      "Home",
                      country_names,
                      xy = c(col, row + 1),
                      colNames = FALSE)

  # Generated: ####
  openxlsx::writeData(wb, "Home",
                      paste("Generated on:", Sys.time()),
                      xy = c(2, row + 2),
                      colNames = FALSE)

  # Package version ####
  openxlsx::writeData(wb, "Home",
                      paste("Package version:",
                            as.character(utils::packageVersion("datapackr"))),
                      xy = c(2, row + 4))

  return(wb)
}
