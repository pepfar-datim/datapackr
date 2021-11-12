#' @export
#' @importFrom utils capture.output
#' @title Unpack a Data Pack sheet.
#'
#' @description Within a submitted Data Pack (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to unpack.
#'
#' @return d
#'
unPackDataPackSheet <- function(d, sheet) {
  ## Helper functions----

  #replaces the original target columns assignment
  getTargetCols <- function(d, sheet) {

    target_cols <- d$info$schema %>%
      dplyr::filter(sheet_name == sheet
                    & (col_type == "target" | (col_type == "result" & dataset == "subnat"))
                    # Filter by what's in submission to avoid unknown column warning messages
                    & indicator_code %in% colnames(d$data$extract)) %>%
      dplyr::pull(indicator_code)

    return(target_cols)
  }

  #replaces the original logic for gathering columns by indicator code
  gatherCols <- function(d, sheet) {

    d$data$extract %<>%
      tidyr::gather(key = "indicator_code",
                    value = "value",
                    -PSNU, -psnuid, -Age, -Sex, -KeyPop, -sheet_name) %>%
      dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value)

    return(d)
  }

  #replaces the original logic for compiling sheets and adding psnuid
  prepForCompilation <- function(d, sheet) {

    d$data$extract %<>%
      addcols(c("KeyPop", "Age", "Sex")) %>%
      # Select only target-related columns
      dplyr::select(PSNU, Age, Sex, KeyPop,
                    dplyr::one_of(target_cols)) %>%
      # Drop rows where entire row is NA
      dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
      # Extract PSNU uid
      dplyr::mutate(
        psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"),
        # Tag sheet name
        sheet_name = sheet
      ) %>%
      dplyr::select(PSNU, psnuid, sheet_name, Age, Sex, KeyPop,
                    dplyr::everything())

    return(d)
  }

  #handle negative values
  checkNegativeValues <- function(d, sheet) {
    negative_values <- d$data$extract %>%
      dplyr::filter(value < 0)

    d$tests$negative_values <- dplyr::bind_rows(d$test$negative_values, negative_values)
    attr(d$tests$negative_values, "test_name") <- "Negative values"

    if (NROW(negative_values) > 0) {

      warning_msg <-
        paste0(
          "ERROR! In tab ",
          sheet,
          ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
          " against FY22 Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
          paste(unique(d$tests$negative_values$indicator_code), collapse = "\n\t* "),
          "\n")

      d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
      d$info$has_error <- TRUE
    }
    return(d)
  }

  ## Scripting----

  #### Read data and preliminary integrity checks ----
  header_row <- headerRow(tool = "Data Pack", cop_year = d$info$cop_year)
  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
      col_types = "text",
      .name_repair = "minimal"
    )

  # If sheet is totally empty, skip
  if (all(is.na(d$data$extract$PSNU))) {
    d$data$extract <- NULL
    return(d)
  }

  # Run structural checks
  d <- checkColStructure(d, sheet)

  # Remove duplicate columns (Take the first example)
  # Make sure no blank column names
  # if tab has no target related content, send d back
  d$data$extract <- d$data$extract[, !duplicated(colnames(d$data$extract))] %>%
    tibble::as_tibble(.name_repair = "unique")
  if (NROW(d$data$extract) == 0) {
    d$data$extract <- NULL
    return(d)
  }

  #List Target Columns, send d back if empty
  target_cols <- getTargetCols(d, sheet)
  if (NROW(target_cols) == 0) {
    d$data$extract <- NULL
    return(d)
  }

  #### Formula checks and data prep ----

  # Add cols to allow compiling with other sheets
  d <- prepForCompilation(d, sheet)

  # TEST: No missing metadata
  d <- checkMissingMetadata(d, sheet)

  # If PSNU has been deleted, drop the row
  d$data$extract %<>% dplyr::filter(!is.na(PSNU))

  # Check for Formula changes
  d <- checkFormulas(d, sheet)

  # Gather all indicators as single column for easier processing
  d <- gatherCols(d, sheet)

  # Drop NAs
  d$data$extract %<>% tidyr::drop_na(value)

  # TEST for non-numeric values
  d <- checkNumericValues(d, sheet)

  # Now that non-numeric cases noted, convert all to numeric & drop non-numeric
  d$data$extract %<>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value) %>%
    dplyr::filter(value != 0) # Filter out zeros

  #### Process sheet types ----

  # TEST that all Prioritizations completed
  if (sheet == "Prioritization") {
    d <- handlePrioritization(d, sheet)
  } else if (sheet == "AGYW") {
    d <- handleAGYW(d, sheet)
  } else if (sheet == "TX") {
    d <- handleTX(d, sheet)
  }

  #### Final testing flow ----

  # TEST: No invalid org units
  d <- checkInvalidOrgUnits(d, sheet)

  # TEST for Negative values
  d <- checkNegativeValues(d, sheet)

  # TEST for Decimal values
  d <- checkDecimalValues(d, sheet)

  # TEST for duplicates
  d <- checkDuplicateRows(d, sheet)

  # TEST for defunct disaggs
  d <- defunctDisaggs(d, sheet)

  #### Aggregations ----

  # Aggregate OVC_HIVSTAT
  d <- aggregateSheet(d, sheet)

  return(d)
}
