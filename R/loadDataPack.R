#' @export
#' @title Load a DataPack from a supplied filepath.
#' @author Scott Jackson
#' @description `loadDataPack` returns a DataPack object conserving styles and
#' formatting of the original Data Pack .xlsx file, as well as other metadata
#' necessary for processing and analyzing data in the Data Pack.
#'
#' @md
#'
#' @inheritParams datapackr_params
#' @param load_sheets Logical. Should all data sheets be loaded too?
#' @param load_wb Should the datapack be loaded as an openxlsx object?
#'
#' @return DataPack object.
#'
loadDataPack <- function(submission_path = NULL,
                         tool = NULL,
                         country_uids = NULL,
                         cop_year = NULL,
                         load_wb = FALSE,
                         load_sheets = FALSE,
                         pzns = NULL,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {

  d <- createKeychainInfo(submission_path = submission_path,
                          tool = tool,
                          country_uids = country_uids,
                          cop_year = cop_year,
                          d2_session = d2_session)

  if (load_wb) {
    d$tool$wb <- openxlsx::loadWorkbook(file = d$keychain$submission_path)
  }

  if (load_sheets) {
    d <- loadSheets(d)
  }

  if (!is.null(pzns)) {
    d$datim$prioritizations <- pzns
  }

  if (interactive()) {
    msg <- paste0("Congratulations. You have loaded a ",
                  "COP", stringr::str_sub(d$info$cop_year, -2, -1),
                  " ", d$info$tool,
                  " for ", d$info$datapack_name, ".")

    interactive_print(msg)
  }

  d

}


#' @export
#' @title Read data from a DataPack object
#' @author Scott Jackson
#' @md
#' @description Reads data from a sheet in a DataPack object. This function is
#' essentially a wrapper for `readxl`'s `read_excel` function, but with additional
#' support for selecting default parameters per DataPack setup.
#'
#' @param d DataPack object, created via `loadDataPack`.
#' @inheritParams readxl::read_excel
#'
#' @return A [tibble][tibble::tibble-package]
#'
readSheet <- function(d,
                      sheet = 1,
                      range = NULL,
                      col_names = TRUE,
                      col_types = "text",
                      na = "",
                      guess_max = 1000,
                      progress = readxl::readxl_progress(),
                      .name_repair = "minimal") {

  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  range <- range %||% readxl::cell_limits(c(header_row, 1), c(NA, NA))

  data <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = range,
      col_names = col_names,
      col_types = col_types,
      na = na,
      guess_max = guess_max,
      progress = progress,
      .name_repair = .name_repair)

  # kill excess rows where all data is NULL, base R for speed
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  # kill empty column names
  keep.cols <- names(data) %in% c("")
  data <- data[, !keep.cols]

  data

}


#' @export
#' @title Load a DataPack from a supplied filepath.
#' @importFrom stats setNames
#' @author Scott Jackson
#' @description `loadSheets` uses `readSheet` to load data from specified sheets
#' into the DataPack `d` object for use in further functions.
#'
#' @md
#'
#' @inheritParams datapackr_params
#' @param sheets Character vector of sheet names to load. The default is NULL
#' which loads all sheets.
#'
#' @return DataPack object.
#'
loadSheets <- function(d,
                       sheets = NULL) {

  sheets <- sheets %missing% NULL
  actual_sheets <- readxl::excel_sheets(d$keychain$submission_path)
  skip <- skip_tabs(tool = d$info$tool, cop_year = d$info$cop_year)$unpack
  sheets_to_read <- actual_sheets[!actual_sheets %in% skip]
  sheets <- sheets %||% sheets_to_read

  # Check Parameters
  sheets <- checkSheets(sheets = sheets,
                        cop_year = d$info$cop_year,
                        tool = d$info$tool,
                        all_sheets = FALSE,
                        operation = "unpack",
                        psnuxim = TRUE)

  # Load Sheets
  extracted_sheets <- lapply(sheets, function(x) readSheet(d, x))
  d$sheets <- setNames(extracted_sheets, sheets)

  d

}
