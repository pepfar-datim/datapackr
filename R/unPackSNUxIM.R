
#' @export
#' @title unPackSNUxIM(d)
#'
#' @description Looks inside submitted Data Pack to extract SNU x IM data from
#'     \code{SNU x IM} tab and restructure this to be ready for cross-
#'     pollination with PSNU-level MER data coming from
#'     \code{\link{unPackSheets}}. This data is also analyzed to identify
#'     structural or data anomalies and print any issues into running Warning
#'     Message queue.
#'
#' @param d Datapackr object

#' @return d
#'
unPackSNUxIM <- function(d) {

  sheet <- "PSNUxIM"

  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

  #Check to see if the object already. If its NULL read it from
  # Excel, otherwise, use the existing object. This is just the
  # first step to be able to functionalize and test everything else
  # below.
  if (is.null(d$data$SNUxIM)) {

    #For hybrid tools, choose the PSNUxIM file.
    psnuxim_path <-
      ifelse(
        !is.null(d$keychain$psnuxim_file_path),
        d$keychain$psnuxim_file_path,
        d$keychain$submission_path
      )

    d$data$SNUxIM <-
      readxl::read_excel(
        path = psnuxim_path,
        sheet = sheet,
        range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
        col_types = "text",
        .name_repair = "minimal"
      )
  }

  d <- checkHasPSNUxIM(d)

  if (!d$info$has_psnuxim) {
    return(d)
  }

  # PATCH: Remove hard-coded FYs
  names(d$data$SNUxIM) <- stringr::str_replace(names(d$data$SNUxIM), " \\(FY22\\)", "")

  d <- d  %>%
    extractSNUxIMCombos(.) %>%
    extractDuplicateRows(., sheet) %>%
    checkColStructure(., sheet)

  #Check invalid disaggs before any deletions or reshaping
  d <- checkPSNUxIMDisaggs(d)

  # Save snapshot of original targets ####

  cols_to_keep <- getColumnsToKeep(d, sheet)
  header_cols <- getHeaderColumns(cols_to_keep, sheet)
  original_targets <- extractOriginalTargets(d, cols_to_keep, header_cols, sheet)

  #TODO: This test is overly simplistic, as we can
  #simply drop blank columns.
  if (NCOL(d$data$SNUxIM) < max(cols_to_keep$col)) {
    stop(
      paste(
        "ERROR: Missing columns in the PSNUxIM tab. Please ensure that there are exactly",
        max(cols_to_keep$col), "columns in the PSNUxIM tab.",
        "Please check columns",
        cellranger::num_to_letter(NCOL(d$data$SNUxIM) + 1),
        "to",
        cellranger::num_to_letter(max(cols_to_keep$col)),
        "."
      )
    )
  }

  if (NCOL(d$data$SNUxIM) > max(cols_to_keep$col)) {
        warning_msg <-
          paste(
            "WARNING: Extra columns in the PSNUxIM tab. Please ensure that there are exactly",
            max(cols_to_keep$col), "columns in the PSNUxIM tab for your final submissions. Please review columns",
            cellranger::num_to_letter(max(cols_to_keep$col) + 1), "to columns",
            cellranger::num_to_letter(NCOL(d$data$SNUxIM))
            )

        d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  d <- checkNonEqualTargets(d, original_targets)

  # Pare down to populated, updated targets only ####
  blank_cols_idx <- which(names(d$data$SNUxIM) == "")
  d$data$SNUxIM <- d$data$SNUxIM[, cols_to_keep$col]
  d$data$SNUxIM <- d$data$SNUxIM[!(names(d$data$SNUxIM) %in% c(""))]

  #Test for missing right side formulas
  d <- testMissingRightSideFormulas(d, cols_to_keep, header_cols, header_row, blank_cols_idx)
  d <- dropDuplicatedPSNUxIMColumns(d)

  # Drop rows where entire row is NA ####
  d$data$SNUxIM %<>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
    tidyr::drop_na(PSNU, indicator_code)

  d <- dropInvalidMechColumns(d, cols_to_keep)

  # TEST: Duplicate Cols; Warn; Combine ####
  d <- combineDuplicatePSNUxIMColumns(d, cols_to_keep)

  # TEST: Non-numeric data. Does not drop any data at this point.
  d <- checkNonNumericPSNUxIMValues(d, header_cols)

  d$data$SNUxIM %<>%
    { suppressWarnings(dplyr::mutate_at(., dplyr::vars(-dplyr::all_of(header_cols$indicator_code)), #nolint
                                       as.numeric))
    }

  d <- testMissingDedupeRollupColumns(d, cols_to_keep)

  d <- recalculateDedupeValues(d)

  d <- testInvalidDedupeValues(d, header_cols)

  d <- testNegativeTargetValues(d, header_cols)

  # Remove all unneeded columns ####
  d$data$SNUxIM %<>%
    dplyr::select(-dplyr::matches("Rollup|Total|MAX|SUM"))

  # Extract PSNU uid ####
  d$data$SNUxIM %<>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")
    ) %>%
    dplyr::select(PSNU, psnuid, indicator_code, Age, Sex, KeyPop,
                  dplyr::everything())

  d <- testInvalidPSNUs(d)

  # Gather all values in single column ####
  # Data is now in LONG format.
  d$data$SNUxIM %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(c(header_cols$indicator_code, "psnuid"))) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mechCode_supportType, value) %>%
    tidyr::drop_na(value)


  d <- testRoundDecimalValues(d)

  d <- testDropPositiveDedupe(d)

  #Remove any potential duplicates by summing.
  #TODO: We should really flag anything which is a duplicate at this point.
  d$data$SNUxIM %<>%
    dplyr::group_by(
      dplyr::across(c(header_cols$indicator_code, "psnuid", "mechCode_supportType"))) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  #Data is in long format here
 d <- calculateFinalDedupeValues(d, header_cols)

 #Note that this function also produces d$info$psnuxim_comparison object
 # which is used in a subsequent function
 d <- testRoundingDiffs(d, original_targets)

 d <- testImbalancedDistribution(d)

  # Rename Dedupe IMs ####
  d$data$SNUxIM %<>%
    dplyr::mutate(
      mechCode_supportType = dplyr::case_when(
        mechCode_supportType == "DSD Dedupe" ~ "00000_DSD",
        mechCode_supportType == "TA Dedupe" ~ "00000_TA",
        mechCode_supportType == "Crosswalk Dedupe" ~ "00001_TA",
        TRUE ~ mechCode_supportType)
    )

  # Drop `Not PEPFAR` data ####
  #TODO: Is there anyway we can get rid of this earlier if we are just
  # dropping the entire column?
  d$data$SNUxIM %<>%
    dplyr::filter(mechCode_supportType != "Not PEPFAR")

 d <- appendUnallocatedData(d)

  # Get mech codes and support types ####
  d$data$SNUxIM %<>%
    tidyr::separate(
      col = mechCode_supportType,
      into = c("mech_code", "support_type"),
      sep = "_",
      remove = TRUE,
      extra = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mech_code, support_type, value)

  return(d)
}
