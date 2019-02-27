#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom utils packageVersion
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
                          as.character(utils::packageVersion("datapackr"))),
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
frameDataSheet <- function(wb, sheet, type = "Data Pack") {
  # Choose schema ####
  if (type == "Data Pack") {
    schema <- datapackr::template_schema
  } else if (type == "Site Tool") {
    schema <- datapackr::site_tool_schema
  }
  
  schema %<>%
  # Filter to current sheet ####
    dplyr::filter(sheet_name == sheet) %>%
    dplyr::select(-sheet_num,-sheet_name)
  
  row_header_cols <- NROW(schema[schema$col_type == "Row Header",])
  
  # Transpose to look like Tool rows ####
  schema %<>% 
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::slice(-1,-2) %>%
  # Add sum rows
    tibble::add_row(V1 = rep(NA_character_,2), .before = 3)
  
  schema[3,row_header_cols] <- "Data Pack"
  schema[4,row_header_cols] <- "Roll-up"
  
  
  # Write rows into Pack sheet ####
  openxlsx::addWorksheet(wb = wb, sheetName = sheet)
  openxlsx::writeData(wb = wb, sheet = sheet, x = schema,
                      xy = c(1,1), colNames = FALSE)
  
  # Write title into Pack sheet ####
  openxlsx::writeData(wb = wb, sheet = sheet, x = sheet,
                      xy = c(1,1), colNames = FALSE)
  
  # Add styles ####
    ## Title
      openxlsx::addStyle(wb = wb, sheet = sheet,
                         style = datapackr::styleGuide$data$title,
                         rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
    ## Header Row
      openxlsx::addStyle(wb = wb, sheet = sheet,
                         style = datapackr::styleGuide$data$header,
                         rows = 1, cols = (row_header_cols+1):length(schema),
                         gridExpand = TRUE, stack = TRUE)
    ## Labels
      openxlsx::addStyle(wb = wb, sheet = sheet,
                         style = datapackr::styleGuide$data$label,
                         rows = 2, cols = (row_header_cols+1):length(schema),
                         gridExpand = TRUE, stack = TRUE)
    ## UIDs
      openxlsx::addStyle(wb = wb, sheet = sheet,
                         style = datapackr::styleGuide$data$uid,
                         rows = 5, cols = (row_header_cols+1):length(schema),
                         gridExpand = TRUE, stack = TRUE)
    ## Row Headers
      openxlsx::addStyle(wb = wb, sheet = sheet,
                         style = datapackr::styleGuide$data$rowHeader,
                         rows = 5, cols = 1:row_header_cols,
                         gridExpand = TRUE, stack = TRUE)

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
#' @param type Either "Data Pack" or "Site Tool". Defaults to "Site Tool".
#' 
#' @return OpenXLSX workbook object for use in data writing functions.
#' 
packFrame <- function(datapack_uid, type = "Data Pack") {
  
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
    wb <- datapackr::writeHomeTab(wb = wb, data_pack_uid = datapack_uid, type = type)
    
  if (type == "Site Tool") {
    # Add Site List tab
      openxlsx::addWorksheet(wb, sheetName = "Site List")
    
    # Add Mech List tab
      openxlsx::addWorksheet(wb, sheetName = "Mechs")
      openxlsx::sheetVisibility(wb)[which(names(wb) == "Mechs")] <- "veryHidden"
  }
    
  # Add validations tab
    openxlsx::addWorksheet(wb, sheetName = "Validations")
    openxlsx::sheetVisibility(wb)[which(names(wb) == "Validations")] <- "veryHidden"
    
  # Frame data tabs ####
    if (type == "Data Pack") {
      schema <- datapackr::template_schema
    } else if (type == "Site Tool") {
      schema <- datapackr::site_tool_schema
    }
    
    sheet_names <- schema %>%
      dplyr::pull(sheet_name) %>%
      unique()
      
    for (i in 1:length(sheet_names)) {
      sheet_name = sheet_names[i]
      wb <- datapackr::frameDataSheet(wb = wb, sheet_name, type = type)
    }
    
  return(wb)
}