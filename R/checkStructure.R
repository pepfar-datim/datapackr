#' @export
#' @title Check tab structure of submitted tool.=
#'
#' @description Checks structural integrity of sheets for submitted tool.
#'
#' @param d Datapackr object
#'
#' @return d
#'
checkStructure <- function(d) {

  # Pull all sheet names from submission as vector
  submission_sheets <- readxl::excel_sheets(d$keychain$submission_path)

  # Pull the unique sheet names from the schema (these are already ordered in the schema so can be called unique)
  sheets_check <- unique(d$info$schema$sheet_name)

  # What columns are missing from the submission?
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)

  missing_sheets <- sheets_check[!sheets_check %in% submission_sheets]

  # If any false, retain vector of missing sheets (retained data frame format)
  d$tests$missing_sheets <- data.frame(sheet_name = missing_sheets)
  attr(d$tests$missing_sheets, "test_name") <- "Missing sheets"

  # Test if data frame has any rows, if so create warning collapsing sheet names otherwise no issues
  if (any(NROW(d$tests$missing_sheets))) {
    warning_msg <-
      paste0(
        "WARNING! MISSING SHEETS: Please ensure no original sheets have",
        " been deleted or renamed in your Data Pack. -> \n  * ",
        paste0(d$tests$missing_sheets$sheet_name, collapse = "\n  * "),
        "\n")
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  d

}
