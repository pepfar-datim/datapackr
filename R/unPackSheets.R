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

  # Check sheets param provided
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
      paste0("You've asked us to unpack the following sheets, which do not ",
             "appear in your submission.:  -> \n\t* ",
             paste(invalid_sheets, collapse = "\n\t* "),
             "\n"))
  }

  # Don't proceed with any sheets where *any* index columns are missing (PSNU,
  #   Age, Sex, KeyPop), or no rows of data
  d <- checkToolEmptySheets(d, sheets = sheets)

  no_data <- c(d$tests$missing_index_columns$sheet_name,
               d$tests$no_rows_data$sheet_name) %>%
    unique()

  sheets <- sheets[!sheets %in% no_data]

  # Check sheet data
  if (check_sheets) {
    d <- checkSheetData(d, sheets = sheets)
  }

  # Unpack Sheet Data ----
  targets <- unPackDataPackSheet(d, sheets)

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
