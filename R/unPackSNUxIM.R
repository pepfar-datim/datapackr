#' Check that a Data Pack tool has a PSNUxIM tab
#'
#' @inheritParams datapackr_params
#' @description Internal function to determine if a PSNUxIM tab exists
#'
#' @return d object list with additional flags for PSNUxIM state.
checkHasPSNUxIM <- function(d) {

  stopifnot(is.data.frame(d$data$SNUxIM))

  if (NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM[[1, 1]])) {
    d$info$has_psnuxim <- FALSE

    if (d$info$tool == "Data Pack") {
      d$info$needs_psnuxim <- TRUE

      warning_msg <-
        paste0(
          "WARNING! Your Data Pack needs a new PSNUxIM tab. Please select `Regenerate PSNUxIM`",
          " to receive an updated copy of your Data Pack with new rows added",
          " to the bottom of your PSNUxIM tab containing any previously missing data combinations.",
          " NOTE that adding data to your PSNUxIM tab could significantly increase the size of your Data Pack, ",
          " so it is recommended to wait to update your Data Pack's PSNUxIM tab until after",
          " all changes to other tabs of your Data Pack are complete.  Once all other updates",
          " are complete, you may return here to update your PSNUxIM tab at any time.",
          "\n")
    } else if (d$info$tool == "OPU Data Pack") {
      warning_msg <- paste0(
        "WARNING! Your OPU Data Pack's PSNUxIM tab appears to be empty. Please",
        " investigate and resubmit."
      )
    }

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")

    return(d)

  } else {
    d$info$has_psnuxim <- TRUE

    return(d)
  }
}

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

  if (d$info$cop_year %in% c(2020, 2021, 2022)) {
    sheet <- "PSNUxIM"
  } else {
    sheet <- "SNU x IM"
  }

  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

  d$data$SNUxIM <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
      col_types = "text",
      .name_repair = "minimal"
    )

  d <- checkHasPSNUxIM(d)

  # PATCH: Remove hard-coded FYs
  names(d$data$SNUxIM) <- stringr::str_replace(names(d$data$SNUxIM), " \\(FY22\\)", "")

  # Document all combos used in submitted PSNUxIM tab ####
  # This ensures tests for new combinations are correctly matched
  d$data$PSNUxIM_combos <- d$data$SNUxIM %>%
    dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop) %>%
    dplyr::mutate(
      psnuid =
        stringr::str_extract(
          PSNU,
          "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")) %>%
    dplyr::distinct() %>%
    dplyr::select(PSNU, psnuid, indicator_code, Age, Sex, KeyPop)

  if (d$info$tool == "Data Pack") {
    d$data$missingCombos <- d$data$MER %>%
      dplyr::filter(!indicator_code %in% c("AGYW_PREV.D.T", "AGYW_PREV.N.T")) %>%
      #Special handling for differences between main tab and PSNUxIM tab age bands
      #The data should not be aggregated at this point. This will happen
      #when the data is repacked by packForDATIM_UndistributedMER
      dplyr::mutate(Age_snuxim = dplyr::case_when(
        stringr::str_detect(Age, "(50-54|55-59|60-64|65+)") &
          !stringr::str_detect(indicator_code, "TX_CURR.T") ~ "50+",
        TRUE ~ Age
      )) %>%
      dplyr::anti_join(
        d$data$PSNUxIM_combos,
        by =  c("PSNU", "psnuid", "indicator_code", "Age_snuxim" = "Age", "Sex", "KeyPop")
      ) %>%
      dplyr::select(-Age_snuxim)

    d$tests$missing_combos <- d$data$missingCombos
    attr(d$tests$missing_combos, "test_name") <- "Missing target combinations"

    d$info$missing_psnuxim_combos <- (NROW(d$data$missingCombos) > 0)

    if (d$info$missing_psnuxim_combos) {
      d$info$needs_psnuxim <- TRUE

      warning_msg <-
        paste0(
          "WARNING! Your Data Pack may need a new PSNUxIM tab. Along with this warning, ",
          " you should also receive an updated copy of your Data Pack with new rows added",
          " to the bottom of your PSNUxIM tab containing any previously missing data combinations.",
          " NOTE that adding data to your PSNUxIM tab could significantly increase the size of your Data Pack, ",
          " so it is recommended to wait to update your Data Pack's PSNUxIM tab until after",
          " all changes to other tabs of your Data Pack are complete.  Once all other updates",
          " are complete, you may return here to update your PSNUxIM tab at any time.",
          "\n")

      d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")

    }
  }

  # TEST: Duplicate Rows; Warn; Combine ####
  duplicates <- d$data$SNUxIM %>%
    dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, DataPackTarget) %>%
    dplyr::filter(DataPackTarget > 0) %>%
    dplyr::select(-DataPackTarget) %>%
    dplyr::group_by(dplyr::across(tidyselect::everything())) %>%
    dplyr::summarise(n = (dplyr::n()), .groups = "drop") %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-n) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::across(tidyselect::everything())) %>%
    dplyr::mutate(sheet = sheet) %>%
    dplyr::select(sheet, dplyr::everything())

  if (NROW(duplicates) > 0) {

    d$tests$duplicate_rows <- dplyr::bind_rows(d$tests$duplicate_rows, duplicates)
    attr(d$tests$duplicate_rows, "test_name") <- "Duplicated rows"

    dupes_msg <-
      utils::capture.output(
        print(as.data.frame(duplicates), row.names = FALSE)
      )

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": DUPLICATE ROWS found. Ensure rows are all unique, and the SNU Disaggregates",
        " are not repeated within tabs. This issue may have been caused by inadvertent",
        " or incorrect copying of data from one row to another. Duplicates are not permitted. -> \n\t",
        paste(dupes_msg, collapse = "\n\t"),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE

  }

  # Run structural checks ####
  d <- checkColStructure(d, sheet)

  # Save snapshot of original targets ####
  cols_to_keep <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  !is.na(indicator_code),
                  !indicator_code %in% c("sheet_num", "ID", "SNU1"),
                  col_type %in% c("row_header", "target"))

  header_cols <- cols_to_keep %>%
    dplyr::filter(col_type == "row_header")


  if (d$info$tool == "Data Pack") {
    original_targets <- d$data$MER
  } else {
    original_targets <- d$data$SNUxIM %>%
      dplyr::select(header_cols$indicator_code, "DataPackTarget") %>%
      { suppressWarnings(dplyr::mutate_at(., dplyr::vars(-dplyr::all_of(header_cols$indicator_code)), # nolint
                                         as.numeric))
      } %>%
      dplyr::group_by(dplyr::across(header_cols$indicator_code)) %>%
      dplyr::summarise(DataPackTarget = sum(DataPackTarget)) %>%
      dplyr::mutate(
        psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"),
        sheet_name = sheet
      ) %>%
      dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop,
                    value = DataPackTarget)
  }

 #  TODO: Reverting this to 5.1.5. We ended up selecting
 #  the FIRST set of mechanism columns which contained the decimal
 #  percentage allocations instead of the second set of columns
 #  which contain the actual values.
 #  #Get the additional mechanisms added by the user
 #  user_mechanisms <- stringr::str_extract(names(d$data$SNUxIM), "\\d{4,}_(DSD|TA)") %>%
 #    purrr::keep(~ !is.na(.x))
 # #Get the mandatory columns
 #  mandatory_columns <- cols_to_keep %>%
 #    dplyr::filter(!is.na(indicator_code)) %>%
 #    dplyr::filter(!indicator_code == "") %>%
 #    dplyr::pull(indicator_code) %>%
 #    purrr::discard(~ .x == "12345_DSD")
 #
 #  d$data$SNUxIM <- d$data$SNUxIM %>%
 #    dplyr::select(mandatory_columns,user_mechanisms)

  # Pare down to populated, updated targets only ####
  d$data$SNUxIM <- d$data$SNUxIM[, cols_to_keep$col]

  d$data$SNUxIM <- d$data$SNUxIM[!(names(d$data$SNUxIM) %in% c(""))]

  # TEST: Missing right-side formulas; Warn; Continue ####
  d$tests$psnuxim_missing_rs_fxs <-
    tidyxl::xlsx_cells(path = d$keychain$submission_path,
                       sheets = "PSNUxIM",
                       include_blank_cells = T) %>%
    dplyr::select(col, row, formula, character) %>%
    dplyr::filter(row >= header_row,
                  col %in% cols_to_keep$col) %>%
    dplyr::filter(!col %in% header_cols$col) %>%
    dplyr::mutate(formula = dplyr::if_else(is.na(formula),
                                           character,
                                           formula)) %>%
    dplyr::select(-character) %>%
    dplyr::filter(is.na(formula)) %>%
    dplyr::mutate(row_letter = openxlsx::int2col(col))

  attr(d$tests$psnuxim_missing_rs_fxs, "test_name") <- "Missing PSNUxIM R.S. Formulas"

  if (NROW(d$tests$psnuxim_missing_rs_fxs) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab PSNUxIM: MISSING FORMULAS ON RIGHT SIDE.",
        " Make sure all formulas in the far right section of your PSNUxIM tab",
        " (section titled 'Target Values') are completely copied to the bottom",
        " of your data. The following columns are implicated. -> \n\t",
        paste(sort(unique(d$tests$psnuxim_missing_rs_fxs$row_letter)), collapse = ", "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  # Drop rows where entire row is NA ####
  d$data$SNUxIM %<>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.)))

  # TEST: Missing key metadata; Error; Drop ####
  # TODO: Make compatible for OPUs
  if (d$info$tool == "Data Pack") {
    d <- checkMissingMetadata(d, sheet)
  }

  # d$data$SNUxIM %<>%
  #   dplyr::filter_at(dplyr::vars(PSNU, indicator_code), dplyr::any_vars(!is.na(.)))

  d$data$SNUxIM %<>%
    tidyr::drop_na(PSNU, indicator_code)

  # nolint
  # nolint start
  # TEST: Improper Col Names; Error; Drop ####
  invalid_mech_headers <- names(d$data$SNUxIM) %>%
    tibble::tibble(col_name = .) %>%
    dplyr::filter(!col_name %in% cols_to_keep$indicator_code,
                  !(stringr::str_detect(col_name, "(\\d){4,6}")
                    & stringr::str_detect(col_name, "DSD|TA")))
  # nolint end

  #Test specifically for DSD and TA which have been populated as mechanisms by the user.
  improper_dedupe_mechs <- names(d$data$SNUxIM) %>%
    tibble::tibble(col_name = .) %>%
    dplyr::filter(!col_name %in% cols_to_keep$indicator_code,
                  (stringr::str_detect(col_name, "^0000[01]")))

  invalid_mech_headers <- dplyr::bind_rows(invalid_mech_headers, improper_dedupe_mechs)

  d$tests$invalid_mech_headers <- data.frame(invalid_mech_headers = invalid_mech_headers$col_name)
  attr(d$tests$invalid_mech_headers, "test_name") <- "Invalid mechanism headers"

  if (NROW(d$tests$invalid_mech_headers) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", INVALID COLUMN HEADERS: Ensure all PSNUxIM column header mechanism are accurate",
        " and complete, and contain at least the 5- or 6-digit mechanism code and either",
        " `DSD` or `TA` (e.g., `12345_DSD`). The following column headers are invalid and",
        " will be dropped in processing. Please use only the form 12345_DSD. ->  \n\t* ",
        paste(d$tests$invalid_mech_headers$invalid_mech_headers, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  d$data$SNUxIM %<>%
    dplyr::select(-dplyr::all_of(d$tests$invalid_mech_headers$invalid_mech_headers))

  # TEST: Duplicate Cols; Warn; Combine ####
  col_names <- names(d$data$SNUxIM) %>%
    tibble::tibble(col_name = .) %>%
    dplyr::mutate(
      # This also creates a standardized mech_code/support_type name
      col_name_new = dplyr::case_when(
        !col_name %in% cols_to_keep$indicator_code
        ~ paste0(stringr::str_extract(col_name, "\\d+"),
                 "_",
                 stringr::str_extract(col_name, "DSD|TA")),
        TRUE ~ col_name),
      id = 1) %>%
    dplyr::group_by(col_name_new) %>%
    dplyr::mutate(
      id = cumsum(id),
      col_name_new = dplyr::case_when(
        id > 1 ~ paste0(col_name_new, "_", id),
        TRUE ~ col_name_new)
    ) %>%
    dplyr::ungroup()

  d$tests$duplicate_cols <- col_names %>%
    dplyr::filter(id > 1) %>%
    dplyr::mutate(
      duplicate_cols = paste0(stringr::str_extract(col_name_new, "\\d+"),
                              "_",
                              stringr::str_extract(col_name_new, "DSD|TA"))) %>%
    dplyr::select(duplicate_cols)

  attr(d$tests$duplicate_cols, "test_name") <- "Duplicate columns"

  if (NROW(d$tests$duplicate_cols) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", DUPLICATE COLUMNS: The following columns appear to be duplicates and",
        " should be consolidated in your submission. While duplicates will be combined",
        " in processing, we cannot guarantee this will work as you might expect. ->  \n\t* ",
        paste(d$tests$duplicate_cols, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  names(d$data$SNUxIM) <- col_names$col_name_new
  # --> This also removes non-essential text from IM name to leave only 12345_DSD format.

  # TEST: Non-numeric data; Warn; Convert & Drop ####
  # TODO: Make compatible for OPUs
  if (d$info$tool == "Data Pack") {
    d <- checkNumericValues(d, sheet, header_cols)
  }

  #sapply(d$data$extract, function(x) which(stringr::str_detect(x, "[^[:digit:][:space:][:punct:]]+")))

  d$data$SNUxIM %<>%
    { suppressWarnings(dplyr::mutate_at(., dplyr::vars(-dplyr::all_of(header_cols$indicator_code)), #nolint
                                       as.numeric))
    }

  # TEST: Missing Dedupe Rollup or Not PEPFAR cols; Error; Add ####
  dedupe_rollup_cols <- cols_to_keep %>%
    dplyr::filter(dataset == "mer" & col_type == "target" & !indicator_code %in% c("", "12345_DSD")) %>%
    dplyr::pull(indicator_code)

  missing_cols_fatal <- dedupe_rollup_cols[!dedupe_rollup_cols %in% names(d$data$SNUxIM)]

  d$tests$missing_cols_fatal <- data.frame(missing_cols_fatal = missing_cols_fatal)
  attr(d$tests$missing_cols_fatal, "test_name") <- "Fatally missing Dedupe Rollup columns"

  if (length(missing_cols_fatal) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", FATALLY MISSING COLUMNS: The following columns are missing, or have",
        " unexpected or blank column headers. Please check your submission. ->  \n\t* ",
        paste(missing_cols_fatal, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  d$data$SNUxIM %<>%
    datapackr::addcols(
      missing_cols_fatal,
      type = "numeric")

  # Recalculate dedupes ####
    ## Other than IM cols, only the following should be considered safe for reuse here:
    # - Deduplicated DSD Rollup
    # - Deduplicated TA Rollup
    # - Total Deduplicated Rollup
    ## All others must be recalculated to protect against formula breakers.

  d$data$SNUxIM %<>%
    datapackr::rowMax(cn = "MAX - TA", regex = "\\d{4,6}_TA") %>%
    datapackr::rowMax(cn = "MAX - DSD", regex = "\\d{4,6}_DSD") %>%
    dplyr::mutate(
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_TA")), na.rm = TRUE),
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_DSD")), na.rm = TRUE),
      `SUM - TA` = `TA Duplicated Rollup`,
      `SUM - DSD` = `DSD Duplicated Rollup`,
      `SUM - Crosswalk Total` =
        rowSums(dplyr::select(.,
                              `Deduplicated DSD Rollup`, `Deduplicated TA Rollup`),
                na.rm = TRUE),
      `MAX - Crosswalk Total` =
        pmax(`Deduplicated DSD Rollup`, `Deduplicated TA Rollup`, na.rm = T),
      `DSD Dedupe` = `Deduplicated DSD Rollup` - `SUM - DSD`,
      `TA Dedupe` = `Deduplicated TA Rollup` - `SUM - TA`,
      `Crosswalk Dedupe` = `Total Deduplicated Rollup` - `SUM - Crosswalk Total`
    )

  # TEST: Improper dedupe values; Error; Continue ####
  dedupe_cols <- names(d$data$SNUxIM)[which(grepl("Deduplicated", names(d$data$SNUxIM)))]

    # Deduplicated DSD within range
  d$tests$dedupes_outside_range <- d$data$SNUxIM %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(dedupe_cols), ~tidyr::replace_na(.x, 0))
    ) %>%
    dplyr::mutate(
      `issues.Deduplicated DSD Rollup` =
        !(`Deduplicated DSD Rollup` >= `MAX - DSD`
          & `Deduplicated DSD Rollup` <= `SUM - DSD`),

      # Deduplicated TA within range
      `issues.Deduplicated TA Rollup` =
        !(`Deduplicated TA Rollup` >= `MAX - TA`
          & `Deduplicated TA Rollup` <= `SUM - TA`),

      # Crosswalk dedupe within range
      `issues.Total Deduplicated Rollup` =
        !(`Total Deduplicated Rollup` >= `MAX - Crosswalk Total`
          & `Total Deduplicated Rollup` <= `SUM - Crosswalk Total`)
    ) %>%
    dplyr::select(dplyr::all_of(c(header_cols$indicator_code, dedupe_cols)),
                  `MAX - DSD`, `SUM - DSD`, `MAX - TA`, `SUM - TA`,
                  `MAX - Crosswalk Total`, `SUM - Crosswalk Total`,
                  tidyselect::matches("issues\\.")) %>%
    dplyr::filter(
      `issues.Deduplicated DSD Rollup`
      | `issues.Deduplicated TA Rollup`
      | `issues.Total Deduplicated Rollup`
    )

  attr(d$tests$dedupes_outside_range, "test_name") <- "Dedupes Outside Acceptable Range"

  if (NROW(d$tests$dedupes_outside_range) > 0) {
    d$info$has_error <- TRUE

    dedupe_issue_cols <-
      tibble::tribble(
        ~col, ~warn,
        "Deduplicated DSD Rollup", any(d$tests$dedupes_outside_range$`issues.Deduplicated DSD Rollup`),
        "Deduplicated TA Rollup", any(d$tests$dedupes_outside_range$`issues.Deduplicated TA Rollup`),
        "Total Deduplicated Rollup", any(d$tests$dedupes_outside_range$`issues.Total Deduplicated Rollup`)
      ) %>%
      dplyr::filter(warn)

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", DEDUPES OUTSIDE ACCEPTABLE RANGE: The following columns contain total",
        " deduplicated targets that are outside acceptable maximum/minimum ranges.",
        " (Your Data Pack notes these with red highlighting.) You must resolve",
        " these issues prior to DATIM import. ->  \n\t* ",
        paste(
          dedupe_issue_cols$col,
          collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }

  # TEST: Negative IM Targets; Error; Drop ####
  d$tests$negative_IM_targets <- d$data$SNUxIM %>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(header_cols$indicator_code)) %>%
    dplyr::filter(stringr::str_detect(mechCode_supportType, "\\d{4,}_(DSD|TA)")
                  & value < 0)

  attr(d$tests$negative_IM_targets, "test_name") <- "Negative Mechanism Targets"

  if (NROW(d$tests$negative_IM_targets) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR!: ",
        NROW(d$tests$negative_IM_targets),
        " cases where negative numbers are being used for mechanism allocations.",
        " The following mechanisms have been affected. These values will be dropped. -> \n\t* ",
        paste(unique(d$tests$negative_IM_targets$mechCode_supportType), collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }

  d$data$SNUxIM %<>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("\\d{4,}_(DSD|TA)"),
        ~ dplyr::if_else(.x < 0, NA_real_, .x))
    )

  # TEST: Formula changes; Warning; Continue ####
  d <- checkFormulas(d, sheet)

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


  # Gather all values in single column ####
  d$data$SNUxIM %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(c(header_cols$indicator_code, "psnuid"))) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mechCode_supportType, value)

  # Drop NAs ####
  d$data$SNUxIM %<>% tidyr::drop_na(value)

  # TEST: Decimals; Error; Round ####
  d$tests$decimals <- d$data$SNUxIM %>%
    dplyr::filter(value %% 1 != 0)

  attr(d$tests$decimals, "test_name") <- "Decimal values"

  if (NROW(d$tests$decimals) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns! These will be rounded. -> \n\t* ",
        paste(unique(d$tests$decimals$mechCode_supportType), collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  d$data$SNUxIM %<>%
    dplyr::mutate(value = round_trunc(value))

  # TEST: Positive Dedupes; Error; Drop ####
  d$tests$positive_dedupes <- d$data$SNUxIM %>%
    dplyr::filter(stringr::str_detect(mechCode_supportType, "Dedupe") & value > 0)
  attr(d$tests$positive_dedupes, "test_name") <- "Positive dedupes"

  if (NROW(d$tests$positive_dedupes) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR!: ",
        NROW(d$tests$positive_dedupes),
        " cases where Deduplicated Rollups are greater than allowed maximum.",
        " You can find these by filtering to positive values in the `DSD Dedupe`, ",
        " `TA Dedupe`, and `Crosswalk Dedupe` columns (columns CX, CY, and CZ) in the PSNUxIM tab.",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }

  d$data$SNUxIM %<>%
    dplyr::filter(!(stringr::str_detect(mechCode_supportType, "Dedupe") & value > 0))

  # Remove unneeded strings from mechanism codes ####
  d$data$SNUxIM %<>%
    dplyr::mutate(
      mechCode_supportType = dplyr::case_when(
        stringr::str_detect(mechCode_supportType, "Dedupe|Not PEPFAR") ~ mechCode_supportType,
        TRUE ~ paste0(stringr::str_extract(mechCode_supportType, "\\d{4,}"),
                      "_",
                      stringr::str_extract(mechCode_supportType, "DSD|TA"))
      )
    )

  d$data$SNUxIM %<>%
    dplyr::group_by(
      dplyr::across(c(header_cols$indicator_code, "psnuid", "mechCode_supportType"))) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # TODO: TEST: Defunct disaggs; Error; Drop ####
  #d <- defunctDisaggs(d, sheet)

  # Drop all zeros against IMs ####
  # d$data$SNUxIM %<>%
  #   dplyr::filter(!(!stringr::str_detect(mechCode_supportType, "Dedupe")
  #                   & value == 0))

  # Drop unneeded Dedupes ####
  d$data$SNUxIM %<>%
    tidyr::pivot_wider(names_from = mechCode_supportType, values_from = value) %>%
    datapackr::addcols(cnames = c("Crosswalk Dedupe", "DSD Dedupe", "TA Dedupe"), type = "numeric")

  ## If only 1 DSD mechanism or only 1 TA mechanism (1 mech total):
  ##   - Do not import any dedupes (Dedupe = NA_real_)
  ## If only 1 DSD mech and only 1 TA mech (2 mechs total):
  ##   - Import Crosswalk Dedupe, whether 0 or <0
  ##   - Do not import any DSD or TA Dedupes (NA_real_)
  ## If >1 DSD mech, but no TA mechs (or vice versa):
  ##   - Import DSD or TA dedupes, whether 0 or <0 (if NA -> 0)
  ##   - Do not import any Crosswalk dedupes
  ## If >1 DSD mech and >1 TA mech:
  ##   - Import all dedupes, whether 0 or <0 (if NA -> 0)

  d$data$SNUxIM %<>%
    dplyr::mutate(
      DSD_count = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_DSD")) >= 1, na.rm = TRUE),
      TA_count = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_TA")) >= 1, na.rm = TRUE),
      Total_count = DSD_count + TA_count,
      `DSD Dedupe` = dplyr::case_when(
        DSD_count <= 1 ~ NA_real_,
        DSD_count > 1 & is.na(`DSD Dedupe`) ~ 0,
        TRUE ~ `DSD Dedupe`
      ),
      `TA Dedupe` = dplyr::case_when(
        TA_count <= 1 ~ NA_real_,
        TA_count > 1 & is.na(`TA Dedupe`) ~ 0,
        TRUE ~ `TA Dedupe`
      ),
      `Crosswalk Dedupe` = dplyr::case_when(
        TA_count == 0 | DSD_count == 0 ~ NA_real_,
        TA_count > 0 & DSD_count > 0 & is.na(`Crosswalk Dedupe`) ~ 0,
        TA_count > 0 & DSD_count > 0 & !is.na(`Crosswalk Dedupe`) ~ `Crosswalk Dedupe`,
        TRUE ~ `Crosswalk Dedupe`
      )
    ) %>%
    dplyr::select(-DSD_count, -TA_count, -Total_count) %>%
    tidyr::pivot_longer(cols = -tidyselect::all_of(c(header_cols$indicator_code, "psnuid")),
                        names_to = "mechCode_supportType",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mechCode_supportType, value)

  # TEST: Rounding Errors; Warn; Continue ####
  #TODO: For OPUs, create d$data$MER from DataPackTarget col? Use above for missing_combos step?
  original_targets %<>%
    dplyr::mutate(
      Age =
        dplyr::case_when(
          (sheet_name %in% c("Cascade", "PMTCT", "TB", "VMMC")
            & indicator_code != "TX_CURR.T"
            & Age %in% c("50-54", "55-59", "60-64", "65+")) ~ "50+",
          TRUE ~ Age)
    ) %>%
    dplyr::group_by(dplyr::across(c(-value))) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  comparison <- d$data$SNUxIM %>%
    dplyr::select(-mechCode_supportType, PSNUxIM_value = value) %>%
    dplyr::group_by(dplyr::across(c(tidyselect::everything(), -PSNUxIM_value))) %>%
    dplyr::summarise(PSNUxIM_value = sum(PSNUxIM_value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::replace_na(list(PSNUxIM_value = 0)) %>%
    dplyr::full_join(.,
      original_targets %>%
        dplyr::select(-sheet_name, DataPack_value = value) %>%
        dplyr::filter(!indicator_code %in% c("AGYW_PREV.D.T", "AGYW_PREV.N.T"))
    ) %>%
    #tidyr::replace_na(list(PSNUxIM_value = 0, DataPack_value = 0)) %>%
    dplyr::filter(is.na(PSNUxIM_value) | is.na(DataPack_value) | PSNUxIM_value != DataPack_value) %>%
    dplyr::mutate(
      diff = dplyr::if_else(is.na(PSNUxIM_value), 0, PSNUxIM_value)
              - dplyr::if_else(is.na(DataPack_value), 0, DataPack_value),
      type =
        dplyr::case_when(
          (is.na(DataPack_value) | DataPack_value == 0)
            & !is.na(PSNUxIM_value) & PSNUxIM_value != 0  ~ "Missing in DP",
          !is.na(DataPack_value) & DataPack_value != 0 & abs(diff) <= 2 ~ "Rounding",
          !is.na(DataPack_value) & DataPack_value != 0 & diff > 2 ~ "Overallocation",
          !is.na(DataPack_value) & DataPack_value != 0 & diff < -2 ~ "Underallocation"
        ))

  d$tests$PSNUxIM_rounding_diffs <- comparison %>%
    dplyr::filter(type == "Rounding") %>%
    dplyr::select(-type)

  attr(d$tests$PSNUxIM_rounding_diffs, "test_name") <- "PSNUxIM Rounding diffs"

  if (NROW(d$tests$PSNUxIM_rounding_diffs) > 0) {
    warning_msg <-
      paste0(
        "WARNING: ",
        NROW(d$tests$PSNUxIM_rounding_diffs),
        " cases where rounding based on PSNUxIM distributions has caused a small",
        " amount of variation from original targets set in other sheets.",
        " A small amount of rounding may be unavoidable given the nature of the",
        " target-setting process. You can review these cases in the FlatPack",
        " provided as an output from this app.",
        " To resolve these cases, please review the PSNUxIM tab to identify and address cases where multiplication of",
        " distribution percentages against Targets has caused rounding error. You may",
        " address this by gradually altering distribution percentages to fine tune",
        " allocations against one or more mechanisms. For additional guidance, see the Data Pack User Guide.",
        "\n"
      )

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  # TEST: Data Pack total not fully distributed to IM ####
  d$tests$imbalanced_distribution <- comparison %>%
    dplyr::filter(type %in% c("Underallocation", "Overallocation")) %>%
    dplyr::select(-type)

  attr(d$tests$imbalanced_distribution, "test_name") <- "Imbalanced distribution"

  if (NROW(d$tests$imbalanced_distribution) > 0) {

    imbalanced_distribution_inds <-  d$tests$imbalanced_distribution %>%
      dplyr::pull(indicator_code) %>%
      unique() %>%
      sort()

    warning_msg <-
      paste0(
        "WARNING!: ",
        NROW(d$tests$imbalanced_distribution),
        " cases where distributed total across all mechanisms and Dedupe is",
        " either more or less than PSNU-level Target.",
        " To identify these, go to your PSNUxIM tab and filter the Rollup column",
        " to find cases where this is not equal to the Data Pack Target column (highlighted red).",
        " NOTE that this may be due to invalid mechanism names in row 14 of your PSNUxIM tab.",
        " For reference, this has affected the following indicators. -> \n\t* ",
        paste(imbalanced_distribution_inds, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    d$info$has_error <- TRUE
  }

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
  d$data$SNUxIM %<>%
    dplyr::filter(mechCode_supportType != "Not PEPFAR")

  # Add Unallocated Data to bottom ####
  d$tests$unallocatedIMs <- comparison %>%
    dplyr::filter(type == "Underallocation") %>%
    dplyr::mutate(value = abs(diff)) %>%
    dplyr::mutate(mechCode_supportType = "Unallocated_DSD") %>%
    dplyr::select(-type, -PSNUxIM_value, -DataPack_value, -diff)

  attr(d$tests$unallocatedIMs, "test_name") <- "Data not yet allocated to IM"

  if (NROW(d$tests$unallocatedIMs) > 0) {
    d$data$SNUxIM %<>%
      rbind(d$tests$unallocatedIMs)

    d$info$unallocatedIMs <- TRUE

    unallocated_inds <-  d$tests$unallocatedIMs %>%
      dplyr::pull(indicator_code) %>%
      unique() %>%
      sort()

    warning_msg <-
      paste0(
        "ERROR!: ",
        NROW(d$tests$unallocatedIMs),
        " cases where targets have not yet been fully allocated to IMs",
        " (excluding AGYW_PREV & cases possibly due to rounding). These data will be",
        " viewable alongside other data in the Data Pack Self-Service App & PAW",
        " Dossiers, but these cannot be imported into DATIM until fully",
        " allocated to IM.",
        " For reference, this has affected the following indicators. -> \n\t* ",
        paste(unallocated_inds, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

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
