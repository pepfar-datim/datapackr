#' @export
#' @title writeSpectrumData
#'
#' @description Utility function mostly for testing which will
#' write Spectrum data in a defined format to the Spectrum tab.
#'
#' @inheritParams datapackr_params
#'
#' @return Modified openxlsx workbook object with Spectrum data written to the
#' Spectrum tab.
writeSpectrumData <- function(wb, spectrum_data, cop_year = NULL) {


  if (!inherits(spectrum_data, "data.frame")) {
    warning("Spectrum data must be a data frame. Proceeding further without writing this data.")
    return(wb)
  }

  expected_names <- c("psnu", "psnu_uid", "area_id", "indicator_code", "dataelement_uid",
                      "age", "age_uid", "sex", "sex_uid", "calendar_quarter", "value", "age_sex_rse", "district_rse")

  if (!identical(names(spectrum_data), expected_names)) {
    warning("Spectrum data does not appear to be in the correct format.Proceeding further without writing this data.")
    return(wb)

  }
  # Write data to sheet ####
  openxlsx::writeData(wb = wb,
                      sheet = "Spectrum",
                      x = spectrum_data, # Object to be written.
                      xy = c(4, 2),
                      colNames = FALSE,
                      rowNames = FALSE,
                      withFilter = FALSE) # Filters are not applied to column name row

  rows_to_write <- seq_len(NROW(spectrum_data)) + 1
  #Write parse age formulas to sheet
  parse_age_fomula <- paste0("=SUBSTITUTE(SUBSTITUTE($I",
                             rows_to_write,
  ",CHAR(61),\"\"),CHAR(34),\"\")")

  openxlsx::writeFormula(
    wb = wb,
    sheet = "Spectrum",
    x = parse_age_fomula,
    startCol = cellranger::letter_to_num("Q"),
    startRow = 2
  )

  # Write IDs to columns
  if (is.null(cop_year)) {
    A1 <- datapackr::toolName_homeCell()
    col <- openxlsx::convertFromExcelRef(A1)
    row <- stringr::str_remove(A1, openxlsx::int2col(col))

    cop_year <- openxlsx::readWorkbook(wb,
                                       sheet = "Home",
                                       rows = row,
                                       cols = col,
                                       colNames = FALSE) %>%
      dplyr::mutate(
        cop_year = stringr::str_extract(X1, "COP\\d{2}"),
        cop_year = as.numeric(stringr::str_replace(cop_year, "COP", "20"))) %>%
      dplyr::pull(cop_year)
  }

  CY <- paste0("CY", cop_year - 1, "Q4")

  id_formulas <-
    paste0("=IF($D", rows_to_write,
           "<>\"\",\"[\"&$E", rows_to_write,
           "&\"]\"&\"|\"&$Q", rows_to_write,
           "&\"|\"&$K", rows_to_write,
           "&\"|\"&$G", rows_to_write,
           "&\"|\"&IF($G", rows_to_write,
           "=\"TX_CURR_SUBNAT.R\",\"", CY, "\",$M", rows_to_write,
           "),\"\")")

  openxlsx::writeFormula(
    wb = wb,
    sheet = "Spectrum",
    x = id_formulas,
    startCol = cellranger::letter_to_num("R"),
    startRow = 2
  )

  wb
}
