#' @export
#' @title Write Home Tab
#'
#' @description
#' Function to write Home tab details into Data Pack as specified.
#'
#' @inheritParams datapackr_params
#'
#' @return Openxlsx workbook object with added, styled Home tab.
#'
writeHomeTab <- function(wb = NULL,
                         datapack_name = NULL,
                         country_uids,
                         cop_year = getCurrentCOPYear(),
                         tool = "Data Pack") {
  #TODO: Setup for default to run PEPFARLANDIA version.

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
  openxlsx::writeData(wb, "Home", "PEPFAR", xy = c(2, 2), colNames = F)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$pepfar,
                     rows = 2, cols = 2)

  # Title ####
  openxlsx::writeData(wb, "Home",
                      x = paste0("COP",
                                 stringr::str_sub(cop_year, -2, -1),
                                 " ",
                                 tool),
                      xy = c(2, 10),
                      colNames = F)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$title,
                     rows = 10, cols = 2)

  # datapack_name ####
  openxlsx::writeData(wb, "Home", datapack_name, xy = c(2, 20), colNames = F)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$datapack_name,
                     rows = 20, cols = 2)

  # country_uids ####
  #TODO: Can we just explicitly state row, col here?
  col <- countryUIDs_homeCell() %>%
    openxlsx::convertFromExcelRef() %>%
    as.numeric()
  row <- countryUIDs_homeCell() %>%
    stringr::str_sub(2, 3) %>%
    as.numeric()

  countries <- paste(country_uids, collapse = ", ")

  openxlsx::writeData(wb, "Home", countries, xy = c(col, row), colNames = F)

  # country_names ####
  country_names <-
    check_params(country_uids = country_uids, datapack_name = NULL) %>%
    purrr::pluck("datapack_name")

  openxlsx::writeData(wb,
                      "Home",
                      country_names,
                      xy = c(col, row + 1),
                      colNames = F)

  # Generated: ####
  openxlsx::writeData(wb, "Home",
                      paste("Generated on:", Sys.time()),
                      xy = c(2, row + 2),
                      colNames = F)

  # Package version ####
  openxlsx::writeData(wb, "Home",
                      paste("Package version:",
                            as.character(utils::packageVersion("datapackr"))),
                      xy = c(2, row + 4))

  return(wb)
}
