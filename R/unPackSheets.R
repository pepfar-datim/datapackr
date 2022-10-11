#' @export
#' @title Unpack data from Data Pack sheets.
#'
#' @description
#' Loops through all critical sheets in a submitted Data Pack
#' and extracts data, then compiles into single flat dataframe.
#'
#' @inheritParams datapackr_params
#' @param check_sheets Logical. Should sheet data be validated?
#' @param separate_datasets Logical. Should datasets be separated?
#'
#' @return d
#'
unPackSheets <- function(d,
                         sheets = NULL,
                         check_sheets = TRUE,
                         separate_datasets = TRUE) {

  interactive_print("Unpacking sheets...")

  if (d$info$tool != "Data Pack") {
    stop("Cannot process that kind of tool. :(")
  }

  # If sheets parameter not provided, use names of sheets in d$sheets
  if (is.null(d$sheets)) {
    d <- loadSheets(d)
  }
  sheets <- sheets %||% grep("PSNUxIM", names(d$sheets), value = TRUE, invert = TRUE)

  sheets <- checkSheets(sheets = sheets,
                        cop_year = d$info$cop_year,
                        tool = d$info$tool,
                        all_sheets = FALSE,
                        psnuxim = FALSE)

  # Check sheets against actual sheets found in d$sheets
  if (!all(sheets %in% names(d$sheets))) {
    invalid_sheets <- unique(sheets[!sheets %in% names(d$sheets)])

    sheets <- sheets[sheets %in% names(d$sheets)]

    interactive_warning(
      paste0("The following sheets do not seem to be present in ",
             "your submission, so cannot be unpacked:  -> \n\t* ",
             paste(invalid_sheets, collapse = "\n\t* "),
             "\n"))
  }

  # Check if there are any sheets where all columns are missing
  has_all_header_columns <- checkExistsAllCols(d, sheets = sheets)
  sheets_with_all_headers <- d$sheets[sheets] %>%
    purrr::keep(has_all_header_columns) %>% names(.)

  lvl <- "ERROR"
  if (any(!has_all_header_columns)) {
    msg <-
      paste0(
        lvl,"! The following sheets could not undergo any of our checks as they
        are missing ALL their column header. Please correct this error and resubmit: ",
        paste(sheets[!has_all_header_columns])
      )

    d$tests$missing_al_columns <- data.frame(sheet_name = sheets[!has_all_header_columns])
    attr(d$tests$missing_al_columns, "test_name") <- "Missing all columns"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

  }

  if (check_sheets) {
    d <- checkSheetData(d, sheets = sheets_with_all_headers)
  }

  # Unpack Sheet Data ----
  targets <- unPackDataPackSheet(d, sheets_with_all_headers)

  # Separate Sheet Data ----
  if (separate_datasets) {
    interactive_print("Separating datasets...")
    datasets <- separateDataSets(data = targets,
                                 cop_year = d$info$cop_year,
                                 tool = d$info$tool)

    d$data$MER <- datasets$MER
    d$data$SUBNAT_IMPATT <- datasets$SUBNAT_IMPATT
  } else {
    d$data$targets <- targets
  }

  return(d)
}
