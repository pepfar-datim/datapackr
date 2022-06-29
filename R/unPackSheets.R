#' @export
#' @title Unpack data from Data Pack sheets.
#'
#' @description
#' Loops through all critical sheets in a submitted Data Pack
#' and extracts data, then compiles into single flat dataframe.
#'
#' @inheritParams datapackr_params
#' @param check_sheets Logical. Should sheet data be validated?
#'
#' @return d
#'
unPackSheets <- function(d,
                         sheets = NULL,
                         check_sheets = TRUE) {

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

  if (check_sheets) {
    d <- checkSheetData(d, sheets = sheets)
  }

  # Unpack Sheet Data ----
  targets <-
    purrr::map_dfr(sheets, function(x)
      unPackDataPackSheet(d, sheet = x))

  # Separate Sheet Data ----
  interactive_print("Separating datasets...")
  datasets <- separateDataSets(data = targets,
                               cop_year = d$info$cop_year,
                               tool = d$info$tool)

  d$data$MER <- datasets$MER
  d$data$SUBNAT_IMPATT <- datasets$SUBNAT_IMPATT

  return(d)
}
