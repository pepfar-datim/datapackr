#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack data into a Data Pack sheet
#' 
#' @description 
#' Packs data into a specified Data Pack sheet
#'
#' @param wb datapackr list object.
#' @param sheet Specified sheet within wb
#' @param org_units 
#' @param schema Defaults to standard Data Pack schema, but allows for provision
#' of custom schema if needed.
#' @param sheet_data Dataset to use as input for packing Data Pack. If left NULL,
#' will produce a Data Pack with orgUnits and disagg specifications, but no data.
#' 
#' @return wb with specified sheet packed with data
#'
packDataPackSheet <- function(wb,
                              sheet,
                              org_units,
                              schema = datapackr::data_pack_schema,
                              sheet_data){ #TODO: Could we load a play dataset here?
  
  # Prepare data for writing to sheet
  sheet_data <- prepareSheetData(sheet = sheet,
                                 org_units = org_units,
                                 schema = schema,
                                 sheet_data = sheet_data)
  
  # Write data to sheet
  openxlsx::writeData(wb = wb,
                      sheet = sheet,
                      x = sheet_data,
                      xy = c(1, headerRow(tool = "Data Pack Template")),
                      colNames = T, rowNames = F, withFilter = TRUE)
  
  return(wb) 
}