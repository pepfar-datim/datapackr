#' @export
#' @title Unpack data from Data Pack sheets.
#'
#' @description
#' Loops through all critical sheets in a submitted Data Pack
#' and extracts data, then compiles into single flat dataframe.
#'
#' @inheritParams datapackr_params
#'
#'
#' @return d
#'
unPackSheets <- function(d, sheets = NULL) {

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

  # Implementing this here instead of in unPackDataPack or unPackTool because
  # while you may want to checkSheetData without running unPackSheets, you should
  # should never unPackSheets without running checkSheetData
  d <- checkSheetData(d, sheets = sheets)

  # Unpack Sheet Data ----
  targets <- NULL

  for (sheet in sheets) {
    interactive_print(sheet)

    extract <- unPackDataPackSheet(d, sheet = sheet)

    if (!is.null(extract)) {
      targets <- dplyr::bind_rows(targets, extract)
    }
  }

  d$data$targets <- targets

  return(d)
}
