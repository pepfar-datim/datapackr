#' @export
#' @importFrom magrittr %>% %<>%
#' @title writeDataSheet(wb, type = "Data Pack")
#' @description 
#' Function to write Data sheet structures 
#' 
#' @param wb Openxlsx workbook object.
#' @param sheet Name of the sheet
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
# Add sum rows ####
    tibble::add_row(V1 = rep(NA_character_,2), .before = 3)
  
  schema[3,row_header_cols] <- "Data Pack Total"
  schema[4,row_header_cols] <- "Site Subtotal"
  
  
# Write rows into Pack sheet ####
  openxlsx::addWorksheet(wb = wb, sheetName = sheet, zoom = 90)
  openxlsx::writeData(wb = wb, sheet = sheet, x = schema,
                      xy = c(1,1), colNames = FALSE)
  
# Write title into Pack sheet ####
  openxlsx::writeData(wb = wb, sheet = sheet, x = sheet,
                      xy = c(1,1), colNames = FALSE)
  
# Add styles ####
  ## Title
      openxlsx::addStyle(wb, sheet,
                         style = datapackr::styleGuide$data$title,
                         rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
  ## Header Row
      openxlsx::addStyle(wb, sheet,
                         style = datapackr::styleGuide$data$header,
                         rows = 1, cols = (row_header_cols+1):length(schema),
                         gridExpand = TRUE, stack = TRUE)
  ## Labels
      openxlsx::addStyle(wb, sheet,
                         style = datapackr::styleGuide$data$label,
                         rows = 2, cols = (row_header_cols+1):length(schema),
                         gridExpand = TRUE, stack = TRUE)
  ## UIDs
      openxlsx::addStyle(wb, sheet,
                         style = datapackr::styleGuide$data$uid,
                         rows = 5, cols = (row_header_cols+1):length(schema),
                         gridExpand = TRUE, stack = TRUE)
  ## Row Headers
      openxlsx::addStyle(wb, sheet,
                         style = datapackr::styleGuide$data$rowHeader,
                         rows = 5, cols = 1:row_header_cols,
                         gridExpand = TRUE, stack = TRUE)
    
  ## Sum row titles
      openxlsx::addStyle(wb, sheet,
                         style = datapackr::styleGuide$data$sumRows,
                         rows = 3:4, cols = row_header_cols,
                         gridExpand = TRUE, stack = TRUE)

    return(wb)
}


#' @export
#' @importFrom magrittr %>% %<>%
#' @title Add validations tab and options to Site tool
#' 
#' @description 
#' Adds "Validations" tabe and all necessary validation options to \code{wb}.
#' 
#' @param wb An Openxlsx workbook object
#'
addValidationsSite <- function(wb) {
  openxlsx::addWorksheet(wb, sheetName = "Validations",visible = FALSE)
  sheet_num <- which(names(wb) == "Validations")
  
# DSD, TA options ####
  openxlsx::writeData(wb, sheet_num,
    x = data.frame(type = c("DSD", "TA")),
    xy = c(1,1),
    colNames = F,
    name = "dsdta"
  )

# Site status ####
  openxlsx::writeData(wb, sheet_num,
    x = data.frame(choices = c("Active","Inactive")),
    xy = c(2,1),
    colNames = F,
    name = "inactive_options"
  )

# Valid Ages ####
  openxlsx::writeData(wb, sheet_num,
    x = datapackr::valid_dp_disaggs$TX$validAges,
    xy = c(1,20),
    colNames = F,
    name = "ages_long"
  )
  
  openxlsx::writeData(wb, sheet_num,
    x = datapackr::valid_dp_disaggs$TB_TX_PREV$validAges,
    xy = c(2,20),
    colNames = F,
    name = "ages_coarse"
  )
  
  openxlsx::writeData(wb, sheet_num,
    x = datapackr::valid_dp_disaggs$OVC$validAges,
    xy = c(3,20),
    colNames = F,
    name = "ages_ovc"
  )
  
  ages <- datapackr::valid_dp_disaggs$TX$validAges
  openxlsx::createNamedRegion(wb, sheet_num, cols = 1,
    rows = which(ages %in% datapackr::valid_dp_disaggs$HTS$validAges) + 19,
    name = "ages_no01"
  )
  
  openxlsx::createNamedRegion(wb, sheet_num, cols = 1,
    rows = which(ages %in% datapackr::valid_dp_disaggs$CXCA$validAges) + 19,
    name = "ages_cxca"
  )
  
  openxlsx::createNamedRegion(wb, sheet_num, cols = 1,
    rows = which(ages %in% datapackr::valid_dp_disaggs$VMMC$validAges) + 19,
    name = "ages_noUnder10"
  )
  
  openxlsx::createNamedRegion(wb, sheet_num, cols = 1,
    rows = which(ages %in% datapackr::valid_dp_disaggs$PrEP$validAges) + 19,
    name = "ages_over15"
  )
  

# Valid Sexes ####
  sexes = data.frame(Sex = c("Female","Male"))
  
  openxlsx::writeData(wb, sheet_num,
    x = sexes,
    xy = c(3,1),
    colNames = F,
    name = "MF"
  )
  
  openxlsx::createNamedRegion(wb, sheet_num,
    cols = 3, rows = 1,
    name = "Female"
  )
  
  openxlsx::createNamedRegion(wb, sheet_num,
    cols = 3, rows = 2,
    name = "Male"
  )

# Valid KPs ####
  openxlsx::writeData(wb, sheet_num,
    x = datapackr::valid_dp_disaggs$KP$validKPs,
    xy = c(4,1),
    colNames = F,
    name = "keypops"
  )
  
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
      creator = "PRIME Information Systems Team"
    )
    
# Modify Base Font ####
    openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 11)
    
# Set global numeric format ####
    options("openxlsx.numFmt" = "#,##0")
    
# Write Home Page ####
   # wb <- writeHomeTab(wb = wb, datapack_uid = datapack_uid, type = type) #TODO: Update parameters
  
# Add Tool specific tabs ####
  if (type == "Site Tool") {
    # Add Site List tab
      openxlsx::addWorksheet(wb, sheetName = "Site List")
    
    # Add Mech List tab
      openxlsx::addWorksheet(wb, sheetName = "Mechs", visible = FALSE)
  }
    
# Add validations ####
  if (type == "Site Tool") {wb <- addValidationsSite(wb)}
    
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
        wb <- frameDataSheet(wb = wb, sheet_name, type = type)
    }
    
  return(wb)
}
