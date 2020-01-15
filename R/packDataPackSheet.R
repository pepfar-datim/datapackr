#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack data into a Data Pack sheet
#' 
#' @description 
#' Packs data into a specified Data Pack sheet
#'
#' @param wb datapackr list object.
#' @param sheet Specified sheet within wb
#' @param org_units Org Units to write in.
#' @param schema Defaults to standard Data Pack schema, but allows for provision
#' of custom schema if needed.
#' @param sheet_data Dataset to use as input for packing Data Pack. If left NULL,
#' will produce a Data Pack with orgUnits and disagg specifications, but no data.
#' @param cop_year COP year for dating as well as selection of
#' templates.
#' 
#' @return wb with specified sheet packed with data
#'
packDataPackSheet <- function(wb,
                              sheet,
                              org_units,
                              schema = datapackr::data_pack_schema,
                              sheet_data,
                              cop_year = cop_year()){ #TODO: Could we load a play dataset here?
  
  # Prepare data for writing to sheet
  sheet_data <- prepareSheetData(sheet = sheet,
                                 org_units = org_units,
                                 schema = schema,
                                 sheet_data = sheet_data,
                                 cop_year = cop_year)
  
  # Write data to sheet
  openxlsx::writeData(wb = wb,
                      sheet = sheet,
                      x = sheet_data,
                      xy = c(1, headerRow("Data Pack Template", cop_year)),
                      colNames = T, rowNames = F, withFilter = TRUE)
  
  # Format percentages
  percentCols <- schema %>%
    dplyr::filter(sheet_name == sheet,
                  value_type == "percentage") %>%
    dplyr::pull(col)
  
  percentStyle = openxlsx::createStyle(numFmt = "0%")
  
  if (length(percentCols) > 0) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       percentStyle,
                       rows = (1:NROW(sheet_data)) + headerRow("Data Pack Template", cop_year),
                       cols = percentCols,
                       gridExpand = TRUE,
                       stack = TRUE)
  }
  
  # Format integers
  integerCols <- schema %>%
    dplyr::filter(sheet_name == sheet,
                  value_type == "integer") %>%
    dplyr::pull(col)
  
  integerStyle = openxlsx::createStyle(numFmt = "#,##0")
  
  if (length(integerCols) > 0) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       integerStyle,
                       rows = (1:NROW(sheet_data)) + headerRow("Data Pack Template", cop_year),
                       cols = integerCols,
                       gridExpand = TRUE,
                       stack = TRUE)
  }
  
  # Format targets
  targetCols <- schema %>%
    dplyr::filter(sheet_name == sheet,
                  col_type == "target") %>%
    dplyr::pull(col)
  
  targetStyle = openxlsx::createStyle(textDecoration = "bold")
  
  if (length(targetCols) > 0) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       targetStyle,
                       rows = (1:NROW(sheet_data)) + headerRow("Data Pack Template", cop_year),
                       cols = targetCols,
                       gridExpand = TRUE,
                       stack = TRUE)
  }
  
  # Hide rows 5-13
  openxlsx::setRowHeights(wb,
                          sheet = sheet,
                          rows = 5:13,
                          heights = 0)
  
  return(wb) 
}
