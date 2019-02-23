#' @export
#' @importFrom magrittr %>% %<>%
#' @title writeHomeTab(wb, data_pack_uid, type = "Data Pack")
#' 
#' @description
#' Function to write Home tab details into either Data Pack or Site Tool, as
#' specified.
#' 
#' @param wb Openxlsx workbook object.
#' @param data_pack_uid Unique ID designating which Data Pack the Home tab should
#' list.
#' @param type Either "Data Pack" or "Site Tool". Defaults to "Data Pack".
#' 
#' @return Openxlsx workbook object with added, styled Home tab.
#' 
writeHomeTab <- function(wb, data_pack_uid, type = "Data Pack") {
  
  # Add Tab ####
    openxlsx::addWorksheet(wb,
                           sheetName = "Home",
                           gridLines = FALSE)
  # PEPFAR banner ####
    openxlsx::writeData(wb, "Home", "PEPFAR", xy = c(2,2), colNames = F)
    openxlsx::addStyle(wb, "Home", datapackr::styleGuide$home$pepfar, rows = 2, cols = 2)
  
  # Title ####
    openxlsx::writeData(wb, "Home", paste0("COP19 ",type), xy = c(2,10), colNames = F)
    openxlsx::addStyle(wb, "Home", datapackr::styleGuide$home$title, rows = 10, cols = 2)
  
  # ou_name ####
    data_pack_name <- datapackr::dataPackMap %>%
      dplyr::filter(model_uid == data_pack_uid) %>%
      dplyr::pull(data_pack_name) %>%
      unique()
    openxlsx::writeData(wb, "Home", data_pack_name, xy = c(2,20), colNames = F)
    openxlsx::addStyle(wb, "Home", datapackr::styleGuide$home$data_pack_name, rows = 20, cols = 2)
  
  # data_pack_uid ####
    openxlsx::writeData(wb, "Home", data_pack_uid, xy = c(2,25), colNames = F)
  
  # Generated: ####
    openxlsx::writeData(wb, "Home", paste("Generated on:", Sys.time()), xy = c(2, 27), colNames = F)
  
  # Package version ####
    openxlsx::writeData(wb,"Home",
                        paste("Package version:",
                          as.character(packageVersion("datapackr"))),
                        xy = c(2, 29))
    
    return(wb)
}


#' @export
#' @importFrom magrittr %>% %<>%
#' @title writeDataSheet(wb, type = "Data Pack")
#' @description 
#' Function to write Data sheet structures 
#' 
#' @param wb Openxlsx workbook object.
#' @param type Either "Data Pack" or "Site Tool". Defaults to "Data Pack".
#' 
#' @return Openxlsx workbook object with added, styled Home tab.
#' 
frameDataSheets <- function(wb, type = "Data Pack") {
  #  ####
  
  return(wb)
}


#' @export
#' @importFrom magrittr %>% %<>%
#' @title Create a Site Tool from scratch and prep it for data.
#' 
#' @description
#' Creates a blank Excel workbook, then packs it with non-data Site Tool
#' elements and features.
#' 
#' @param datapack_uid A unique ID specifying the PEPFAR Operating Unit 
#' the Site Tool belongs to.
#' @param type Either "Data Pack" or "Site Tool". Defaults to "Data Pack".
#' 
#' @return OpenXLSX workbook object for use in data writing functions.
#' 
frameSiteTool <- function(datapack_uid, type = "Data Pack") {
  
  # Create Workbook ####
    wb <- openxlsx::createWorkbook(
      creator = "PRIME Information Systems Team",
      title = paste0("PEPFAR ",type)
    )
    
  # Modify Base Font ####
    openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 11)
    
  # Set global numeric format ####
    options("openxlsx.numFmt" = "#,##0")
    
  # Write Home Page ####
    wb <- writeHomeTab(wb = wb, data_pack_uid = datapack_uid, type = type)
    
  # Add Site List tab
    openxlsx::addWorksheet(wb, sheetName = "Site List")
    
  # Add Mech List tab
    openxlsx::addWorksheet(wb, sheetName = "Mechs")
    openxlsx::sheetVisibility(wb)[which(sheets == "Mechs")] <- "veryHidden"
    
  # Add validations tab
    openxlsx::addWorksheet(wb, sheetName = "Validations")
    openxlsx::sheetVisibility(wb)[which(sheets == "Validations")] <- "veryHidden"
    
  # Frame data tabs ####
    wb <- frameDataSheets(wb = wb, type = type)
    
  return(wb)
}