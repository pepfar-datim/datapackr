#' @export
#' @importFrom magrittr %>% %<>%
#' @title writeDataSheet(wb, tool = "Data Pack")
#' @description
#' Function to write Data sheet structures
#'
#' @param wb Openxlsx workbook object.
#' @param sheet Name of the sheet
#' @param tool Defaults to "Data Pack".
#'
#' @return Openxlsx workbook object with added, styled Home tab.
#'
frameDataSheet <- function(wb, sheet, tool = "Data Pack") {
# Choose schema ####
  if (tool == "Data Pack") {
      schema <- datapackr::template_schema
  }

  schema %<>%
# Filter to current sheet ####
    dplyr::filter(sheet_name == sheet) %>%
    dplyr::select(-sheet_num, -sheet_name)

  row_header_cols <- NROW(schema[schema$col_type == "Row Header", ])

# Transpose to look like Tool rows ####
  schema %<>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::slice(-1, -2) %>%
# Add sum rows ####
    tibble::add_row(V1 = rep(NA_character_, 2), .before = 3)

  schema[3, row_header_cols] <- "Data Pack Total"
  schema[4, row_header_cols] <- "Site Subtotal"


# Write rows into Pack sheet ####
  openxlsx::addWorksheet(wb = wb, sheetName = sheet, zoom = 90)
  openxlsx::writeData(wb = wb, sheet = sheet, x = schema,
                      xy = c(1, 1), colNames = FALSE)

# Write title into Pack sheet ####
  openxlsx::writeData(wb = wb, sheet = sheet, x = sheet,
                      xy = c(1, 1), colNames = FALSE)

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
#' @title Create a Tool from scratch and prep it for data.
#'
#' @description
#' Creates a blank Excel workbook, then packs it.
#'
#' @param datapack_uid A unique ID specifying the PEPFAR Operating Unit
#' the Tool belongs to.
#' @param tool Defaults to "Data Pack".
#'
#' @return OpenXLSX workbook object for use in data writing functions.
#'
packFrame <- function(datapack_uid, tool = "Data Pack") {

# Create Workbook ####
    wb <- openxlsx::createWorkbook(
      creator = "PRIME Information Systems Team"
    )

# Modify Base Font ####
    openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 11)

# Set global numeric format ####
    options("openxlsx.numFmt" = "#,##0")

# Write Home Page ####
   # wb <- writeHomeTab(wb = wb, datapack_uid = datapack_uid, tool = tool) #TODO: Update parameters

# Frame data tabs ####
    if (tool == "Data Pack") {
        schema <- datapackr::template_schema
    }

    sheet_names <- schema %>%
      dplyr::pull(sheet_name) %>%
      unique()

    for (i in seq_along(sheet_names)) {
        sheet_name <- sheet_names[i]
        wb <- frameDataSheet(wb = wb, sheet_name, tool = tool)
    }

  return(wb)
}
