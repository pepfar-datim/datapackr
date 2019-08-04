#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom utils packageVersion
#' @title writeHomeTab(wb, datapack_uid, type = "Data Pack")
#' 
#' @description
#' Function to write Home tab details into either Data Pack or Site Tool, as
#' specified.
#' 
#' @param wb Openxlsx workbook object.
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#' @param country_uids List of 11 digit alphanumeric DATIM codes representing
#' countries.
#' @param type Either "Data Pack" or "Site Tool". Defaults to "Data Pack".
#' 
#' @return Openxlsx workbook object with added, styled Home tab.
#' 
writeHomeTab <- function(wb, datapack_name, country_uids, type = "Data Pack") {
  #TODO: Setup for default to run PEPFARLANDIA version.
  
  # Add Tab ####
  if(!stringr::str_detect(names(wb), "Home")) {
    openxlsx::addWorksheet(wb,
                           sheetName = "Home",
                           gridLines = FALSE)
  }
  
  # PEPFAR banner #### 
  # TODO: Decide whether to keep features for writing styles, given we'll be creating Site Tool from template now too
  openxlsx::writeData(wb, "Home", "PEPFAR", xy = c(2,2), colNames = F)
  openxlsx::addStyle(wb, "Home",
                     styleGuide$home$pepfar,
                     rows = 2, cols = 2)
  
  # Title ####
  openxlsx::writeData(wb, "Home",
                      x = paste0("COP",
                                 stringr::str_sub(cop_year(), -2,-1),
                                 " ",
                                 type),
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
  col <- countryUIDs_homeCell() %>%
    openxlsx::convertFromExcelRef() %>%
    as.numeric()
  row <- countryUIDs_homeCell() %>%
    stringr::str_sub(2,3) %>%
    as.numeric()
  
  openxlsx::writeData(wb, "Home", country_uids, xy = c(col, row), colNames = F)
  
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
  
  # COP Year ####
  openxlsx::writeData(wb, "Home",
                      cop_year(),
                      xy = c(2, row+6))
  
  return(wb)
}
