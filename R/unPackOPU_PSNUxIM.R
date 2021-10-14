#' @export
#' @title Unpack data from OPU Data Pack PSNUxIM tab.
#'
#' @description
#' Extracts data from updated targets in OPU Data Pack PSNUxIM tab.
#'
#' @param d Datapackr object
#'
#' @return d
#'
unPackOPU_PSNUxIM <- function(d) {
  # NOTES:
    # Unsure whether I need to wait so long to pivot_longer if I'm just going to drop all 0s anyway
  # Preamble ####
  if (d$info$tool != "OPU Data Pack") {
    stop("Cannot process that kind of tool. :(")
  }

  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  sheet <- "PSNUxIM"

  cols_to_keep <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  !is.na(indicator_code),
                  !indicator_code %in% c("12345_DSD", "12345_TA"),
                  col_type %in% c("row_header", "target"))

  header_cols <- cols_to_keep %>%
    dplyr::filter(col_type == "row_header")

  # Extract data ####
  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
      col_types = "text",
      .name_repair = "minimal"
    )

  if (NROW(d$data$extract) == 1 & is.na(d$data$extract[[1, 1]])) {
    d$info$has_psnuxim <- FALSE
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "WARNING! Your OPU Data Pack appears to be missing all data on the `PSNUxIM` tab.",
        " This is a fatal error. Please contact the Help Desk for guidance.",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")

    return(d)

  } else {
    d$info$has_psnuxim <- TRUE
  }

  # TODO: Check column structures ####
    # d <- checkColStructure(d, sheet)

  # Pare down to updated targets only ####
  d$data$extract <-
    d$data$extract[c(cols_to_keep$col, (max(cols_to_keep$col) + 1):NCOL(d$data$extract))]

  # TEST: Blank Col Names; Error; Drop ####
  blank_col_headers <- names(d$data$extract)[which(nchar(names(d$data$extract)) == 0)]

  if (length(blank_col_headers) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", BLANK COLUMN HEADERS: The submission contains ",
        length(blank_col_headers),
        " columns with data, but no column header. For IM columns, please add a column header of the form 12345_DSD.",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }

  d$data$extract <- d$data$extract[!(names(d$data$extract) %in% c(""))]

  # Drop columns with all NA ####
  d$data$extract %<>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::select_if(function(x) any(!is.na(x)))

  #TEST: Invalid indicator codes
  d <- .testInvalidIndicatorCodes(d)

  # TEST: Improper Col Names; Error; Drop ####
  invalid_mech_headers <- names(d$data$extract) %>%
    tibble::tibble(col_name = .) %>%
    dplyr::filter(!col_name %in% cols_to_keep$indicator_code,
                  !(stringr::str_detect(col_name, "(\\d){4,6}") #nolint
                    & stringr::str_detect(col_name, "DSD|TA")))

  d$tests$invalid_mech_headers <- data.frame(invalid_mech_headers = invalid_mech_headers$col_name)
  attr(d$tests$invalid_mech_headers, "test_name") <- "Invalid mechanism headers"

  if (NROW(d$tests$invalid_mech_headers) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", INVALID COLUMN HEADERS: The following column headers are invalid and",
        " will be dropped in processing. Please use only the form 12345_DSD. ->  \n\t* ",
        paste(d$tests$invalid_mech_headers$invalid_mech_headers, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }

  d$data$extract %<>%
    dplyr::select(-dplyr::all_of(d$tests$invalid_mech_headers$invalid_mech_headers))

  # TEST: Duplicate Cols; Warn; Combine ####
  col_names <- names(d$data$extract) %>%
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

  names(d$data$extract) <- col_names$col_name_new
      # --> This also removes non-essential text from IM name to leave only 12345_DSD format.

  # TEST: Non-numeric data; Warn; Convert & Drop ####
  d <- checkNumericValues(d, sheet, header_cols)

    #sapply(d$data$extract, function(x) which(stringr::str_detect(x, "[^[:digit:][:space:][:punct:]]+")))

  d$data$extract %<>%
    { suppressWarnings(dplyr::mutate_at(., dplyr::vars(-dplyr::all_of(header_cols$indicator_code)), # nolint
                     as.numeric))
    }

  # Drop rows where entire row is NA ####
  d$data$extract %<>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.)))

  # TODO TEST: No missing metadata ####
    #d <- checkMissingMetadata(d, sheet)

  # TEST: Missing Dedupe Rollup cols; Error; Add ####
  dedupe_rollup_cols <- cols_to_keep %>%
    dplyr::filter(dataset == "mer" & col_type == "target") %>%
    dplyr::pull(indicator_code)

  missing_cols_fatal <- dedupe_rollup_cols[!dedupe_rollup_cols %in% names(d$data$extract)]

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
  }

  d$data$extract %<>%
    datapackr::addcols(
      missing_cols_fatal,
      type = "numeric")

  # If PSNU or indicator_code has been deleted, drop the row ####
  d$data$extract %<>%
    tidyr::drop_na(PSNU, indicator_code)

  # Recalculate dedupes ####
    ## Other than IM cols, only the following should be considered safe for reuse here:
      # - Deduplicated DSD Rollup
      # - Deduplicated TA Rollup
      # - Total Deduplicated Rollup
    ## All others must be recalculated to protect against formula breakers.

  rowMax <- function(df, cn, regex) {
      df[[cn]] <- df %>%
        dplyr::select(tidyselect::matches(match = regex)) %>%
        dplyr::mutate(default = 0) %>% # included to make sure there is at least 1 column
        purrr::pmap(pmax, na.rm = T) %>%
        as.numeric

      return(df)
    }

  d$data$extract %<>%
    rowMax(cn = "Min Deduplicated TA Rollup", regex = "\\d{4,6}_TA") %>%
    rowMax(cn = "Min Deduplicated DSD Rollup", regex = "\\d{4,6}_DSD") %>%
    dplyr::mutate(
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,6}_TA")), na.rm = TRUE),
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,6}_DSD")), na.rm = TRUE),
      `Max Deduplicated TA Rollup` = `TA Duplicated Rollup`,
      `Max Deduplicated DSD Rollup` = `DSD Duplicated Rollup`,
      `Max Total Deduplicated Rollup` =
        rowSums(dplyr::select(.,
                              `Deduplicated DSD Rollup`, `Deduplicated TA Rollup`),
                na.rm = TRUE),
      `Min Total Deduplicated Rollup` =
          pmax(`Deduplicated DSD Rollup`, `Deduplicated TA Rollup`, na.rm = T),
      `DSD Dedupe` = `Deduplicated DSD Rollup` - `DSD Duplicated Rollup`,
      `TA Dedupe` = `Deduplicated TA Rollup` - `TA Duplicated Rollup`,
      `Crosswalk Dedupe` = `Total Deduplicated Rollup` - `Max Total Deduplicated Rollup`
    )

  # TEST: Improper dedupe values; Error; Continue ####

  dedupe_cols <- names(d$data$extract)[which(grepl("Deduplicated", names(d$data$extract)))]

    # Deduplicated DSD within range
  d$tests$dedupes_outside_range <- d$data$extract %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(dedupe_cols), ~tidyr::replace_na(.x, 0))
    ) %>%
    dplyr::mutate(
      `issues.Deduplicated DSD Rollup` =
        !(`Deduplicated DSD Rollup` >= `Min Deduplicated DSD Rollup`
         & `Deduplicated DSD Rollup` <= `Max Deduplicated DSD Rollup`),

    # Deduplicated TA within range
        `issues.Deduplicated TA Rollup` =
          !(`Deduplicated TA Rollup` >= `Min Deduplicated TA Rollup`
           & `Deduplicated TA Rollup` <= `Max Deduplicated TA Rollup`),

    # Crosswalk dedupe within range
        `issues.Total Deduplicated Rollup` =
          !(`Total Deduplicated Rollup` >= `Min Total Deduplicated Rollup`
           & `Total Deduplicated Rollup` <= `Max Total Deduplicated Rollup`)
    ) %>%
    dplyr::select(dplyr::all_of(c(header_cols$indicator_code, dedupe_cols)),
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
        " (The OPU Data Pack notes these with red highlighting.) You must resolve",
        " these issues prior to DATIM import. ->  \n\t* ",
        paste(
          dedupe_issue_cols$col,
          collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  # TEST: Negative IM Targets; Error; Drop ####
  d$tests$negative_IM_targets <- d$data$extract %>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(header_cols$indicator_code)) %>%
    dplyr::filter(stringr::str_detect(mechCode_supportType, "\\d{4,6}_(DSD|TA)")
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
    d$info$has_error <- TRUE
  }

  d$data$extract %<>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("\\d{4,6}_(DSD|TA)"),
        ~ dplyr::if_else(.x < 0, NA_real_, .x))
    )

  # TODO: Check for Formula changes ####

  # Remove all unneeded columns ####
  d$data$extract %<>%
    dplyr::select(-dplyr::matches("Rollup"))

  # Gather all values in single column ####
  d$data$extract %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(header_cols$indicator_code)) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code),
                  mechCode_supportType, value)

  # Drop NAs ####
  d$data$extract %<>% tidyr::drop_na(value)

  # TEST: Decimals; Error; Round ####
  d$tests$decimals <- d$data$extract %>%
    dplyr::filter(value %% 1 != 0)

  attr(d$tests$decimals, "test_name") <- "Decimal values"

  if (NROW(d$tests$decimals) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns! These will be rounded. -> \n\t* ",
        paste(unique(d$tests$decimals$mechCode_supportType), collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }

  d$data$extract %<>%
    dplyr::mutate(value = round_trunc(value))

  # TEST: Positive Dedupes; Error; Drop ####
  d$tests$positive_dedupes <- d$data$extract %>%
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
        " `TA Dedupe`, and `Crosswalk Dedupe` columns (columns CX, CY, and CZ) in the PSNUxIM tab.")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }

  d$data$extract %<>%
    dplyr::filter(!(stringr::str_detect(mechCode_supportType, "Dedupe") & value > 0))

  # TEST: Duplicate Rows; Warn; Combine ####
  d$data$extract %<>%
    dplyr::mutate(
      mechCode_supportType = dplyr::case_when(
        stringr::str_detect(mechCode_supportType, "Dedupe") ~ mechCode_supportType,
        TRUE ~ paste0(stringr::str_extract(mechCode_supportType, "\\d{4,6}"),
                      "_",
                      stringr::str_extract(mechCode_supportType, "DSD|TA"))
      )
    )

  d <- checkDuplicateRows(d, sheet)
    ## This may be a repeat of information already shared in checking duplicate
    ## columns, but may also catch rows that were duplicates even before pivot_longer.

  d$data$extract %<>%
    dplyr::group_by(
      dplyr::across(c(header_cols$indicator_code, "mechCode_supportType"))) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # TODO: TEST: Defunct disaggs; Error; Drop ####
    #d <- defunctDisaggs(d, sheet)

  # Drop all zeros against IMs ####
  d$data$extract %<>%
   dplyr::filter(!(!stringr::str_detect(mechCode_supportType, "Dedupe")
                   & value == 0))

  # Drop unneeded Dedupes ####
  d$data$extract %<>%
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

  d$data$extract %<>%
    dplyr::mutate(
      DSD_count = rowSums(dplyr::select(., tidyselect::matches("\\d{4,6}_DSD")) >= 1, na.rm = TRUE),
      TA_count = rowSums(dplyr::select(., tidyselect::matches("\\d{4,6}_TA")) >= 1, na.rm = TRUE),
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
    tidyr::pivot_longer(cols = -tidyselect::all_of(header_cols$indicator_code),
                        names_to = "mechCode_supportType",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code),
                  mechCode_supportType, value)

  # Rename Dedupe IMs ####
  d$data$extract %<>%
    dplyr::mutate(
      mechCode_supportType = dplyr::case_when(
        mechCode_supportType == "DSD Dedupe" ~ "00000_DSD",
        mechCode_supportType == "TA Dedupe" ~ "00000_TA",
        mechCode_supportType == "Crosswalk Dedupe" ~ "00001_TA",
        TRUE ~ mechCode_supportType)
    )

  # Get mech codes and support types ####
  d$data$extract %<>%
    tidyr::separate(
      col = mechCode_supportType,
      into = c("mech_code", "support_type"),
      sep = "_",
      remove = TRUE,
      extra = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code),
                  mech_code, support_type, value)

  # Extract PSNU uid ####
  d$data$extract %<>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")
    ) %>%
    dplyr::select(PSNU, psnuid, indicator_code, Age, Sex, KeyPop,
                  dplyr::everything())

  return(d)

}

#' @export
#' @title Test for invalid Indicator Codes
#'
#' @description
#' Tests for invalid Indicator Codes
#'
#' @param d Datapackr object
#'
#' @return d
#'
.testInvalidIndicatorCodes <- function(d) {
  #Test any invalid indicator codes

  indicator_codes_sheet <- d$data$extract %>%
    dplyr::select(indicator_code) %>%
    dplyr::distinct()

  if (d$info$cop_year == 2020) {
    indicator_codes_schema <- datapackr::cop20_data_pack_schema %>%
      dplyr::filter(dataset == "mer", col_type == "target") %>%
      dplyr::select(indicator_code) %>%
      dplyr::mutate(is_valid = TRUE)
  } else if (d$info$cop_year == 2021) {
    indicator_codes_schema <- cop21_data_pack_schema %>%
      dplyr::filter(dataset == "mer", col_type == "target") %>%
      dplyr::select(indicator_code) %>%
      dplyr::mutate(is_valid = TRUE)
  }

  invalid_indicator_codes <- dplyr::left_join(indicator_codes_sheet,
                                              indicator_codes_schema) %>%
    dplyr::filter(is.na(is_valid))

  d$tests$invalid_indicator_codes <- invalid_indicator_codes
  attr(d$tests$invalid_indicator_codes, "test_name") <-
    "Invalid indicator codes"

  if (NROW(d$tests$invalid_indicator_codes) > 0) {
    d$info$has_error <- TRUE
    
    warning_msg <-
      paste0(
        "ERROR! INVALID INDICATOR CODES: The following indicator codes are invalid",
        " will be dropped in processing. ->  \n\t* ",
        paste(d$tests$invalid_indicator_codes$indicator_code, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }
  return(d)
}
