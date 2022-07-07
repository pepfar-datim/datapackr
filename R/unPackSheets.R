#' @export
#' @title Unpack data from Data Pack sheets.
#'
#' @description
#' Loops through all critical sheets in a submitted Data Pack
#' and extracts data, then compiles into single flat dataframe.
#'
#' @inheritParams datapackr_params
#' @return d
#'
unPackSheets <- function(d, d2_session = dynGet("d2_default_session", inherits = TRUE)) {

  # Get sheets list
  sheets <- d$info$schema %>%
    dplyr::filter(
      !sheet_name %in% c(skip_tabs(tool = d$info$tool, cop_year = d$info$cop_year),
                         "SNU x IM", "PSNUxIM")) %>%
    dplyr::pull(sheet_name) %>%
    unique()

  actual_sheets <- readxl::excel_sheets(d$keychain$submission_path)
  sheets_to_read <- sheets[sheets %in% actual_sheets]

  d$data$targets <- NULL

  for (sheet in sheets_to_read) {
    interactive_print(sheet)

    if (d$info$tool == "Data Pack") {
      d <- unPackDataPackSheet(d, sheet = sheet, d2_session = d2_session)
    } else {
      stop("Cannot process that kind of tool. :(")
    }

    if (!is.null(d$data$extract)) {
      d$data$targets <- dplyr::bind_rows(d$data$targets, d$data$extract)
    }
  }

  return(d)
}
