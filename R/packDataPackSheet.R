#' @export
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
                              schema = pick_schema(),
                              sheet_data,
                              cop_year = NULL) { #TODO: Could we load a play dataset here?

  cop_year %<>% check_cop_year()

  # Prepare data for writing to sheet ####
  sheet_data <- prepareSheetData(sheet = sheet,
                                 org_units = org_units,
                                 schema = schema,
                                 sheet_data = sheet_data,
                                 cop_year = cop_year)

  # Write data to sheet ####
  openxlsx::writeData(wb = wb,
                      sheet = sheet,
                      x = sheet_data, # Object to be written.
                      xy = c(1, headerRow("Data Pack Template", cop_year)), # Defines start column and start row.
                      colNames = TRUE,
                      rowNames = FALSE,
                      withFilter = FALSE) # Filters are not applied to column name row

  # Format targets ####
  targetCols <- schema %>%
    dplyr::filter(sheet_name == sheet,
                  col_type == "target") %>%
    dplyr::pull(col)

  targetStyle <- openxlsx::createStyle(textDecoration = "bold")

  if (length(targetCols) > 0) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       targetStyle,
                       rows = (seq_len(NROW(sheet_data))) + headerRow("Data Pack Template", cop_year),
                       cols = targetCols,
                       gridExpand = TRUE,
                       stack = TRUE)
  }

  # Format percentages ####
  percentCols <- schema %>%
    dplyr::filter(sheet_name == sheet,
                  value_type == "percentage") %>%
    dplyr::filter(!indicator_code %in%
                    c("HIV_PREV.T_1", "Incidence_SUBNAT.Rt.T_1",
                      "Incidence_SUBNAT.Rt.T", "KP_ESTIMATES.Prev.T_1")) %>%
    dplyr::pull(col)

  percentStyle <- openxlsx::createStyle(numFmt = "0%") # cell formatting

  if (length(percentCols) > 0) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       percentStyle,
                       rows = (seq_len(NROW(sheet_data))) + headerRow("Data Pack Template", cop_year),
                       cols = percentCols,
                       gridExpand = TRUE, # styling applied to all rows and cols.
                       stack = TRUE)# New style is merged with existing cell styles.
  }

  # Format HIV_PREV ####
  if (sheet %in% c("Epi Cascade I", "Cascade", "PMTCT", "VMMC", "KP")) {
    percentDecimalCols <- schema %>%
      dplyr::filter(sheet_name == sheet,
                    indicator_code %in%
                      c("HIV_PREV.NA.Age/Sex/HIVStatus.T", "HIV_PREV.T_1",
                        "Incidence_SUBNAT.Rt.T_1", "Incidence_SUBNAT.Rt.T",
                        "KP_ESTIMATES.Prev.T")) %>%
      dplyr::pull(col)

    percentDecimalStyle <- openxlsx::createStyle(numFmt = "0.00%") # cell formatting

    openxlsx::addStyle(wb,
                       sheet = sheet,
                       percentDecimalStyle,
                       rows = (seq_len(NROW(sheet_data))) + headerRow("Data Pack Template", cop_year),
                       cols = percentDecimalCols,
                       gridExpand = TRUE, # styling applied to all rows and cols.
                       stack = TRUE) # New style is merged with existing cell styles.
  }

  # Format integers ####
  integerCols <- schema %>%
    dplyr::filter(sheet_name == sheet,
                  value_type == "integer") %>% # extract all integer columns
    dplyr::pull(col) # extracts single column.

  integerStyle <- openxlsx::createStyle(numFmt = "#,##0") # cell formatting

  if (length(integerCols) > 0) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       integerStyle,
                       rows = (seq_len(NROW(sheet_data))) + headerRow("Data Pack Template", cop_year),
                       cols = integerCols,
                       gridExpand = TRUE, # styling applied to all rows and cols.
                       stack = TRUE) # New style is merged with existing cell styles.
  }

  # Hide rows 5-13 ####
  openxlsx::setRowHeights(wb,
                          sheet = sheet,
                          rows = 5:13,
                          heights = 0)

  return(wb)
}
