#' @export
#' @title Unpack data from Data Pack sheets.
#'
#' @description
#' Loops through all critical sheets in a submitted Data Pack
#' and extracts data, then compiles into single flat dataframe.
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
unPackSheets <- function(d, sheets = NULL) {
  
  if (d$info$tool != "Data Pack") {
    stop("Cannot unpack sheets for that kind of tool.")
  }

  actual_sheets <- readxl::excel_sheets(d$keychain$submission_path)
  skip = c(skip_tabs(tool = d$info$tool, cop_year = d$info$cop_year), "PSNUxIM")
  sheets_to_read <- actual_sheets[!actual_sheets %in% skip]
  sheets <- sheets %||% sheets_to_read
  
  # Check Parameters
  sheets <- checkSheets(sheets = sheets,
                        cop_year = d$info$cop_year, 
                        tool = d$info$tool,
                        all_sheets = FALSE,
                        psnuxim = FALSE)

  for (sheet in sheets_to_read) {
    interactive_print(sheet)

    d <- unPackDataPackSheet(d, sheet = sheet)

    if (!is.null(d$data[[as.character(sheet)]])) {
      d$data$targets <- dplyr::bind_rows(d$data$targets, d$data[[as.character(sheet)]])
    }
  }

  return(d)
}
