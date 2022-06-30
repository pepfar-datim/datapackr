#' Perform various data quality checks on in-process data from Data Packs.
#'
#' @description
#' A series of functions to check and validate quality & integrity
#' of data encountered in various `unPack...` functions across `datapackr`. Note
#' that these functions do not attempt to correct issues identified, but only to
#' identify them.
#'
#' `checkToolStructure` checks structural integrity of sheets for submitted tool.
#'
#' `checkToolComments` searches Data Pack for any comments that cause
#' corruption when executing openxlsx::saveWorkbook.
#'
#' `checkToolConnections` detects the presence of any external links in a Tool.
#'
#' `checkDupeRows` checks for any rows with duplicates across PSNU and other key
#'    disaggregates.
#'
#' `checkColumnStructure` checks structural integrity of columns on critical sheets for
#'    a submitted Data Pack.
#'
#' `checkNonNumeric` alerts to non-numeric values instead of valid data.
#'
#' `checkMissingMetadata` alerts to missing Age, Sex, or KeyPop.
#'
#' `checkNegativeValues` alerts to negative values.
#'
#' `checkDecimalValues` alerts to decimal values where these are unallowed.
#'
#' `checkInvalidOrgUnits` alerts to invalid org units.
#'
#' `checkInvalidPrioritizations` alerts to invalid Prioritizations.
#'
#' `checkFormulas` checks formulas in a specified sheet in a submitted Data Pack
#' to make sure they are up to date and have not been tampered with.
#'
#' `checkDisaggs` alerts to invalid disaggs (Age, Sex, KeyPop).
#'
#' Some functions (`checkToolStructure`, `checkToolComments`, &
#' `checkToolConnections`) are designed to check across entire tools at
#' once. All others are specific to single sheet.
#'
#' @name unPackDataChecks
#' @md
#'
#' @param d DataPack object loaded via `loadDataPack`.
#' @param sheet String. Name of DataPack sheet to check data from. Default is
#'   first sheet.
#' @param quiet Logical. Should warning messages be printed? Default is TRUE.
#'
#' @return A DataPack object, with updated tests and warnings.
#'
NULL


#' @export
#' @rdname unPackDataChecks
#'
checkDupeRows <- function(d,
                          sheet,
                          quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  # Get data
  data <- d$sheets[[as.character(sheet)]]

  # Get header_cols
  header_cols <- d$info$schema %>%
    dplyr::filter(
      sheet_name == sheet,
      col_type == "row_header") %>%
    dplyr::pull(indicator_code) %>%
    c(., "mechCode_supportType")

  header_cols <- header_cols[header_cols %in% names(data)]

  # Drop rows/cols with all NAs or 0s
  # We don't care if these are duplicates
  names(data) <- data %>%
    names() %>%
    make.names() %>%
    make.unique()

  data %<>% {
      suppressWarnings(
        dplyr::mutate(.,
                      dplyr::across(-tidyselect::all_of(header_cols), #nolint
                                    as.numeric)))
    } %>%
    dplyr::mutate(
      dplyr::across(-tidyselect::all_of(header_cols),
                    ~tidyr::replace_na(.x, 0))) %>%
    dplyr::filter(
      rowSums(dplyr::across(-tidyselect::all_of(header_cols))) != 0)

  data <-
    dplyr::bind_cols(
      dplyr::select(data, tidyselect::all_of(header_cols)),
      dplyr::select(data, -tidyselect::all_of(header_cols)) %>%
        dplyr::select_if(colSums(.) != 0))

  # Duplicates
  dupes <- data %>%
    #dplyr::select(header_cols) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(header_cols))) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-dplyr::any_of(c("n", "ID"))) %>%
    dplyr::arrange(dplyr::across()) %>%
    dplyr::mutate(sheet = sheet) %>%
    dplyr::select(sheet, dplyr::everything())

  if (NROW(dupes) > 0) {
    lvl <- "ERROR"

    dupes_msg <-
      capture.output(
        print(as.data.frame(dupes), row.names = FALSE))

    msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": DUPLICATE ROWS found. Ensure PSNUs or Age, Sex, KeyPop disaggregates",
        " are not repeated within tabs. This issue may have been caused by inadvertent",
        " or incorrect copying of data from one row to another. -> \n\t",
        paste(dupes_msg, collapse = "\n\t"),
        "\n")

    d$tests$duplicate_rows %<>% dplyr::bind_rows(dupes)
    attr(d$tests$duplicate_rows, "test_name") <- "Duplicated rows"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}


#' @export
#' @rdname unPackDataChecks
#'
checkColumnStructure <- function(d,
                                 sheet,
                                 quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  # Get data
  data <- d$sheets[[as.character(sheet)]]

  # Cross-check cols
  submission_cols <- names(data) %>%
    tibble::enframe(name = "submission_order", value = "indicator_code")

  schema_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet)

  if (sheet == "PSNUxIM") {
    ## Drop all IM cols (left & right sides)
    schema_cols %<>%
      dplyr::filter(
        col_type != "allocation",
        !(col_type == "target"
          & (indicator_code %in% c("Not PEPFAR", "12345_DSD", ""))))

    ## We don't care to track col issues with blank/NA cols in PSNUxIM
    submission_cols %<>%
      dplyr::filter(!is.na(indicator_code),
                    !indicator_code %in% c("")) %>%
      ## Standardize mech/support_type names
      dplyr::mutate(
        indicator_code =
          dplyr::case_when(
            stringr::str_detect(indicator_code, "\\d+")
            ~ paste0(stringr::str_extract(indicator_code, "\\d+"),
                     "_",
                     stringr::str_extract(indicator_code, "DSD|TA")),
            TRUE ~ indicator_code))
  }

  col_check <- schema_cols %>%
    dplyr::select(indicator_code, template_order = col) %>%
    dplyr::full_join(submission_cols,
                     by = "indicator_code") %>%
    dplyr::mutate(sheet = sheet) %>%
    dplyr::select(sheet, dplyr::everything())

  # Missing ----
  if (any(is.na(col_check$submission_order))) {
    lvl <- "WARNING"

    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::select(sheet, indicator_code)

    msg <-
      paste0(
        lvl, "! In tab ", sheet,
        ", MISSING COLUMNS: Please ensure no columns have been deleted or renamed from",
        " the original Data Pack you have received. ->  \n\t* ",
        paste(missing_cols$indicator_code, collapse = "\n\t* "),
        "\n")

    d$tests$missing_cols <- dplyr::bind_rows(d$tests$missing_cols, missing_cols)
    attr(d$tests$missing_cols, "test_name") <- "Missing columns"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }

  }

  # Duplicates ----
  dup_cols <- col_check %>%
    dplyr::filter(!is.na(submission_order)) %>%
    dplyr::mutate(critical = !is.na(template_order))

  if (sheet == "PSNUxIM") {
    last_im_int <- dup_cols %>%
      dplyr::filter(critical) %>%
      dplyr::mutate(
        jump = submission_order - dplyr::lag(submission_order, default = 0)) %>%
      dplyr::filter(jump > 1) %>%
      dplyr::pull(submission_order) %>%
      min() - 1

    # first_im_int <- max(dup_cols$template_order, na.rm = T)
    # b4_im <- dup_cols$submission_order[which(dup_cols$template_order == first_im_int)]

    dup_cols %<>%
      dplyr::mutate(
        grp =
          dplyr::case_when(
            critical ~ "Critical",
            submission_order < last_im_int ~ "IM Allocations",
            TRUE ~ "IM Targets"))
  }

  dup_cols %<>%
    dplyr::select(sheet, indicator_code, critical, dplyr::any_of("grp")) %>%
    dplyr::add_count(dplyr::across(c(indicator_code, dplyr::any_of("grp")))) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-dplyr::any_of("grp"), -n) %>%
    dplyr::distinct()

  if (NROW(dup_cols) > 0) {
    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! In tab ", sheet,
        ", DUPLICATE COLUMNS: The following columns appear multiple times. This",
        " must be resolved in your submission, especially for those columns",
        " noted as [Critical!]. ->  \n\t* ",
        paste(
          paste0(dup_cols$indicator_code,
                 ifelse(dup_cols$critical, " [Critical!]", "")),
          collapse = "\n\t* "),
        "\n")

    dup_cols %<>%
      dplyr::mutate(duplicated_cols = TRUE) %>% # remove after deprecate checkColStructure
      dplyr::select(sheet, indicator_code, duplicated_cols)

    d$tests$duplicate_columns %<>% dplyr::bind_rows(dup_cols)
    attr(d$tests$duplicate_columns, "test_name") <- "Duplicate columns"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  # Out of order ----
  out_of_order <- col_check %>%
    dplyr::filter(!is.na(template_order), # Only care about critical cols
                  !is.na(submission_order), # We've already caught missing
                  template_order != submission_order) %>%
    dplyr::rename(columns_out_of_order = indicator_code)

  if (NROW(out_of_order) > 0) {
    lvl <- "WARNING"

    msg <-
      paste0(
        lvl, "! In tab ", sheet,
        ", OUT OF ORDER COLUMNS: While it is permitted to rearrange columns",
        " within your Data Pack as needed, this is not encouraged as it may",
        " introduce unintended formula errors. Please review these columns to",
        " ensure their rearrangement has not caused any issues. -> \n\t* ",
        paste(out_of_order$indicator_code, collapse = "\n\t* "),
        "\n")

    d$tests$columns_out_of_order %<>% dplyr::bind_rows(out_of_order)
    attr(d$tests$columns_out_of_order, "test_name") <- "Columns out of order"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  # TODO: Add PSNUxIM check for malformed IM/type headers
  # TODO: Add PSNUxIM check for making sure IM appears once in both L & R

  if (!quiet) {
    printMessages(messages)
  }

  return(d)

}



#' @export
#' @rdname unPackDataChecks
#'
checkToolStructure <- function(d, quiet = TRUE) {

  interactive_print("Checking structure...")

  if (!quiet) {
    messages <- MessageQueue()
  }

  interactive_print("Checking for any missing tabs...")

  submission_sheets <- readxl::excel_sheets(d$keychain$submission_path)
  schema_sheets <- unique(d$info$schema$sheet_name)
  missing_sheets <- schema_sheets[!schema_sheets %in% submission_sheets]

  if (length(missing_sheets) > 0) {

    lvl <- "WARNING"

    msg <-
      paste0(
        lvl, "! MISSING SHEETS: Please ensure no original sheets have",
        " been deleted or renamed in your Data Pack. -> \n  * ",
        paste0(missing_sheets, collapse = "\n  * "),
        "\n")

    d$tests$missing_sheets <- data.frame(sheet_name = missing_sheets)
    attr(d$tests$missing_sheets, "test_name") <- "Missing sheets"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)

}



#' @export
#' @rdname unPackDataChecks
#'
checkToolComments <- function(d, quiet = TRUE) {

  interactive_print("Checking comments...")

  if (!quiet) {
    messages <- MessageQueue()
  }

  # if (is.null(d$tool$wb)) {
  #   wb <- openxlsx::loadWorkbook(file = d$keychain$submission_path)
  # } else {
  #   wb <- d$tool$wb
  # }

  if (is.null(d$info$workbook_contents)) {
    d <- listWorkbookContents(d)
  }

  d$info$has_comments_issue <- any(grepl("xl/threadedComments/", d$info$workbook_contents))

  # d$info$has_comments_issue <- any(sapply(wb$threadComments, length) != 0)

  if (d$info$has_comments_issue) {

    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! Your workbook contains at least one case of a new type of comment
        introduced in Office 365 called a 'Threaded Comment'. This type of comment,
        as opposed to the previous type of Notes used in Microsoft Excel, causes
        corruption issues when this app attempts to update your PSNUxIM tab.
        Prior to submitting for an updated PSNUxIM tab, you MUST remove all
        threaded comments. For more information about the differences between
        threaded comments and notes,",
        "see: https://support.office.com/en-us/article/the-difference-between-threaded-comments-and-notes-75a51eec-4092-42ab-abf8-7669077b7be3", # nolint
        "\n")

    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }

  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)

}


#' @export
#' @rdname unPackDataChecks
#'
checkToolConnections <- function(d, quiet = TRUE) {

  interactive_print("Checking external links...")

  if (!quiet) {
    messages <- MessageQueue()
  }

  if (is.null(d$info$workbook_contents)) {
    d <- listWorkbookContents(d)
  }

  d$info$has_external_links <-
    any(grepl("xl/externalLinks/externalLink\\d+\\.xml", d$info$workbook_contents))

  if (d$info$has_external_links) {

    lvl <- "WARNING"

    msg <-
      paste0(
        lvl, "! Your workbook contains at least one external link. ",
        "This usually results from copying and pasting from another workbook. ",
        "Please find and remove the external links in your DataPack. ",
        "This error may result in other validation checks failing to run properly ",
        "and should be fixed immediately.",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }

  }

  if (!quiet) {
    printMessages(messages)
  }

  d

}


#' @export
#' @rdname unPackDataChecks
#'
checkNonNumeric <- function(d, sheet, quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  # Get data
  data <- unPackDataPackSheet(d = d,
                              sheet = sheet,
                              clean_orgs = TRUE,
                              clean_disaggs = TRUE,
                              clean_values = FALSE)

  # keep_cols <- d$info$schema %>%
  #   dplyr::filter(sheet_name == sheet,
  #                 !is.na(indicator_code),
  #                 !indicator_code %in% c("sheet_num", "ID", "SNU1"),
  #                 col_type %in% c("row_header", "target", "result"))

  # if (d$info$tool == "OPU Data Pack") {
  #   data %<>%
  #     tidyr::gather(key = "mechCode_supportType",
  #                   value = "value",
  #                   -tidyselect::all_of(header_cols$indicator_code)) %>%
  #     dplyr::select(dplyr::all_of(header_cols$indicator_code),
  #                   mechCode_supportType, value) %>%
  #     tidyr::drop_na(value)
  # }

  # if (d$info$tool == "Data Pack" & sheet == "PSNUxIM" & d$info$cop_year %in% c(2021, 2022)) {
  #   data %<>%
  #     tidyr::gather(key = "mechCode_supportType",
  #                   value = "value",
  #                   -tidyselect::all_of(c(header_cols$indicator_code))) %>%
  #     dplyr::select(dplyr::all_of(header_cols$indicator_code), -indicator_code,
  #                   indicator_code = mechCode_supportType, value) %>%
  #     tidyr::drop_na(value)
  # }

  # header_cols <- keep_cols %>%
  #   dplyr::filter(col_type == "row_header") %>%
  #   dplyr::pull(indicator_code)

  non_numeric <- data %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(value_numeric = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(is.na(value_numeric)) %>%
    dplyr::mutate(sheet = sheet) %>%
    dplyr::select(sheet, dplyr::everything(), -value_numeric)

  if (NROW(non_numeric) > 0) {
    lvl <- "WARNING"

    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ": NON-NUMERIC VALUES found! Please check the following columns for",
        " possible non-numeric values. ->  \n\t* ",
        paste(sort(unique(non_numeric$indicator_code)), collapse = "\n\t* "),
        "\n")

    d$tests$non_numeric %<>% dplyr::bind_rows(non_numeric)
    attr(d$tests$non_numeric, "test_name") <- "Non-numeric values"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}


#' @export
#' @rdname unPackDataChecks
#'
checkMissingMetadata <- function(d, sheet, quiet = T) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  # Get data
  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {

    data <- d$sheets[["PSNUxIM"]]
  } else {
    data <- d$sheets[[as.character(sheet)]]
  }

  # Munge
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

  missing_metadata <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row = dplyr::row_number() + header_row,
                  sheet = sheet) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("^PSNU$|^ID$|^indicator_code$")),
                     dplyr::any_vars(is.na(.)))
  # NOTE: Checks for missing Age, Sex, KP are performed in defunctDisaggs, not here.

  # TEST
  if (NROW(missing_metadata) > 0) {
    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ", MISSING PSNU, INDICATOR_CODE, OR ID: Review any tabs flagged by this test",
        " to investigate whether PSNU, Age, Sex, or Key Population identifier",
        " information data have been deleted.",
        NROW(missing_metadata),
        " rows where blank entries exist in the PSNU, indicator_code, or ID columns.",
        " Note that blank entries in these columns will prevent processing of",
        " data in that row. The following rows are affected: ",
        paste(missing_metadata$row, collapse = ", "),
        "\n")

    d$tests$missing_metadata <- dplyr::bind_rows(d$tests$missing_metadata, missing_metadata)
    attr(d$tests$missing_metadata, "test_name") <- "Missing metadata"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}



#' @export
#' @rdname unPackDataChecks
#'
checkNegativeValues <- function(d, sheet, quiet = T) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  negative_values <- unPackDataPackSheet(d,
                                         sheet,
                                         clean_orgs = TRUE,
                                         clean_disaggs = TRUE,
                                         clean_values = FALSE) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(value < 0)

  if (NROW(negative_values) > 0) {
    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
        " against Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
        paste(sort(unique(negative_values$indicator_code)), collapse = "\n\t* "),
        "\n")

    d$tests$negative_values <- dplyr::bind_rows(d$test$negative_values, negative_values)
    attr(d$tests$negative_values, "test_name") <- "Negative values"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}



#' @export
#' @rdname unPackDataChecks
#'
checkDecimalValues <- function(d, sheet, quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  data <- unPackDataPackSheet(d,
                              sheet,
                              clean_orgs = TRUE,
                              clean_disaggs = TRUE,
                              clean_values = FALSE)

  decimals_allowed <- d$info$schema %>%
    dplyr::filter(
      sheet_name == sheet
      & col_type == "target"
      # Filter by what's in submission to avoid unknown column warning messages
      & indicator_code %in% unique(data$indicator_code)
      & value_type == "percentage"
    ) %>%
    dplyr::pull(indicator_code)

  decimal_cols <- data %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(value %% 1 != 0
                  & !indicator_code %in% decimals_allowed)

  if (NROW(decimal_cols) > 0) {
    lvl <- "WARNING"

    msg <-
      paste0(
        lvl,
        "! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns that should have only",
        " whole, positive, numeric values. These will be rounded. -> \n\t* ",
        paste(sort(unique(decimal_cols$indicator_code)), collapse = "\n\t* "),
        "\n")

    d$tests$decimal_values <- dplyr::bind_rows(d$tests$decimal_cols, decimal_cols)
    attr(d$tests$decimal_values, "test_name") <- "Decimal values"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}


#' @export
#' @rdname unPackDataChecks
#'
checkInvalidOrgUnits <- function(d, sheet, quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  # Get data
  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {

    data <- d$sheets[["PSNUxIM"]]
  } else {
    data <- unPackDataPackSheet(d = d,
                                sheet = sheet,
                                clean_orgs = FALSE,
                                clean_disaggs = FALSE,
                                clean_values = TRUE)
  }

  # TEST
  expected_PSNUs <- valid_PSNUs %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    dplyr::select(psnu, psnu_uid)

  invalid_orgunits <- data %>%
    dplyr::select(PSNU) %>%
    tidyr::drop_na(PSNU) %>%
    dplyr::distinct() %>%
    dplyr::mutate(psnuid = extract_uid(PSNU),
                  sheet_name = sheet) %>%
    dplyr::left_join(datapackr::valid_PSNUs, by = c("psnuid" = "psnu_uid")) %>%
    dplyr::filter(is.na(psnu) | is.na(psnuid)) %>%
    dplyr::select(PSNU)

  na_orgunits <- data %>%
    dplyr::select(PSNU) %>%
    dplyr::filter(is.na(PSNU))

  if (NROW(invalid_orgunits) > 0 | (NROW(na_orgunits) > 0 & NROW(data) > 0)) {

    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ", INVALID OR BLANK ORG UNITS: ",
        ifelse(NROW(na_orgunits) > 0,
               paste0("There are ", NROW(na_orgunits), " rows where PSNU/DSNU is blank."),
               ""),
        " Please also review the below PSNUs/DSNUs with invalid or missing org",
        " unit UIDs. (This is an 11-digit alphanumeric code assigned in DATIM to",
        " each organization unit.) If you believe these are valid, confirm in",
        " both DATIM & FACTS Info that the below are correctly added and active",
        " for the appropriate COP Year. ->  \n\t* ",
        paste(invalid_orgunits$PSNU, collapse = "\n\t* "),
        "\n")

    d$tests$invalid_orgunits <- dplyr::bind_rows(d$tests$invalid_orgunits, invalid_orgunits)
    attr(d$tests$invalid_orgunits, "test_name") <- "Invalid orgunits"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}


#' @export
#' @rdname unPackDataChecks
checkInvalidPrioritizations <- function(d, sheet, quiet = T) {

  if (!sheet == "Prioritization") {
    return(d)
  }

  if (!quiet) {
    messages <- MessageQueue()
  }

  # Get data
  data <- unPackDataPackSheet(d,
                              sheet,
                              clean_orgs = TRUE,
                              clean_disaggs = FALSE,
                              clean_values = FALSE)

  expected_PSNUs <- valid_PSNUs %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    dplyr::mutate(psnu_name = paste0(psnu, " [", psnu_uid, "]")) %>%
    dplyr::select(psnu_uid, psnu_name)

  invalid_prioritizations <- data %>%
    dplyr::select(PSNU, psnuid, value) %>%
    dplyr::full_join(expected_PSNUs,
                     by = c("psnuid" = "psnu_uid")) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_detect(PSNU, "^_Military") ~ "Military",
        is.na(PSNU) ~ "Missing PSNU",
        is.na(value) ~ "Blank",
        !value %in% prioritization_dict()$value ~ "Invalid")) %>%
    dplyr::filter(!is.na(type),
                  type != "Military") %>%
    dplyr::select(psnu_name, type)

  if (NROW(invalid_prioritizations) > 0) {

    inv_pzs_msg <-
      utils::capture.output(
        print(as.data.frame(invalid_prioritizations), row.names = FALSE))

    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ": INVALID PRIORITIZATIONS: The following PSNUs have been assigned",
        " invalid or blank prioritizations. Please note that all PSNUs must have",
        " an assigned prioritization, and prioritizations can only be assigned ",
        paste_oxford(prioritization_dict()$value, final = "or"), ". -> \n\t",
        paste(inv_pzs_msg, collapse = "\n\t"),
        "\n")

    d$tests$invalid_prioritizations <- invalid_prioritizations
    attr(d$tests$invalid_prioritizations, "test_name") <- "Invalid prioritizations"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}



#' @export
#' @rdname unPackDataChecks
checkFormulas <- function(d, sheet, quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  header_row <- headerRow(tool = "Data Pack", cop_year = d$info$cop_year)

  # Pull in formulas from schema
  # TODO: Flag differently for green vs other columns
  formulas_schema <- d$info$schema %>%
    dplyr::filter(
      sheet_name == sheet,
      !is.na(formula)) %>%
    dplyr::select(col, indicator_code, formula) %>%
    # tidyr::crossing(row = ((header_row+1):max(formulas_datapack$row))) %>%
    # dplyr::select(row, col, indicator_code, formula) %>%
    # dplyr::mutate(
    #   formula =
    #     stringr::str_replace_all(
    #       formula,
    #       pattern = paste0("(?<=[:upper:])", header_row+1),
    #       replacement = as.character((header_row+1):max(formulas_datapack$row))
    #     )
    # )
    dplyr::mutate(
      formula = stringr::str_replace_all(
        formula,
        "(?<=[:upper:])\\d+",
        "\\\\d+"))

  # Pull in formulas from Data Pack sheet
  formulas_datapack <-
    tidyxl::xlsx_cells(path = d$keychain$submission_path,
                       sheets = sheet,
                       include_blank_cells = T) %>%
    # Note that this function won't pick up any blank column headers or deleted formulas
    dplyr::filter(row >= header_row) %>%
    dplyr::select(sheet, row, col, character, numeric, formula) %>%
    dplyr::mutate(formula = dplyr::if_else(is.na(formula),
                                           as.character(numeric),
                                           formula),
                  formula = dplyr::if_else(is.na(formula), character, formula))

  # Remove duplicate columns (Take 1st)
  cols_to_keep <- formulas_datapack %>%
    dplyr::filter(row == header_row) %>%
    dplyr::select(col, character) %>%
    dplyr::filter(!duplicated(character)) %>%
  # Drop cols not in schema
    dplyr::filter(character %in% formulas_schema$indicator_code)

  formulas_datapack %<>%
    dplyr::right_join(
      (cols_to_keep %>%
         dplyr::select(col, indicator_code = character)),
      by = c("col" = "col")) %>%
    dplyr::select(row, col, indicator_code, formula) %>%
    dplyr::filter(row != header_row) %>%
    purrr::when(
      sheet == "PSNUxIM" & d$info$tool == "Data Pack" ~ .,
      ~  dplyr::group_by(., row) %>%
        dplyr::mutate(occurrence = duplicated(indicator_code)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(occurrence == FALSE) %>%
        dplyr::select(-occurrence) %>%
        # Limit to only columns that DUIT cares about
        dplyr::filter(indicator_code %in% formulas_schema$indicator_code)
    ) %>%
    #TODO: Add to catch where referencing wrong row
    #TODO: Fix to catch where formulas are completed deleted
    dplyr::mutate(
      formula = stringr::str_replace_all(
        formula,
        "(?<=[:upper:])\\d+",
        "\\\\d+"))

  # Compare formulas from schema against Data Pack to see diffs
  altered_formulas <- formulas_schema %>%
    dplyr::filter(indicator_code %in% formulas_datapack$indicator_code) %>%
    dplyr::left_join(
      formulas_datapack,
      by = ifelse(sheet == "PSNUxIM" & d$info$tool == "Data Pack",
                  c("col" = "col"),
                  c("indicator_code" = "indicator_code"))) %>%
    dplyr::filter(formula.x != formula.y) %>%
    purrr::when(sheet == "PSNUxIM" & d$info$tool == "Data Pack" ~ dplyr::rename(., indicator_code = indicator_code.y),
                ~ .) %>%
    dplyr::select(
      indicator_code, correct_fx = formula.x, submitted_fx = formula.y, row) %>%
    dplyr::group_by(indicator_code, correct_fx, submitted_fx) %>%
    dplyr::mutate(count = dplyr::n()) %>%
    dplyr::group_by(indicator_code, correct_fx, submitted_fx, count) %>%
    dplyr::summarise(affected_rows = formatSetStrings(row)) %>%
    dplyr::ungroup()

  if (NROW(altered_formulas) > 0) {

    lvl <- "WARNING"

    cols_affected <- altered_formulas %>%
      dplyr::select(indicator_code, correct_fx, count) %>%
      dplyr::group_by(indicator_code, correct_fx) %>%
      dplyr::summarize(count = sum(count)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(fx_violations = paste0(indicator_code, ":  ", count))

    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ", ", NROW(cols_affected), " ALTERED FORMULAS:",
        " Altering formulas in the Grey colored columns without DUIT and PPM",
        " approval may lead to programmatic and technical issues in your Data ",
        " Pack. Green colored columns may be altered without permission. ",
        " Note that this warning may be triggered due to a formula being deleted",
        " or overwritten, or a manual fix not being applied.",
        " Affected columns and the number of violations are listed below. ->  \n\t* ",
        paste(cols_affected$fx_violations, collapse = "\n\t* "),
        "\n")

    d$tests$altered_formulas <-
      dplyr::bind_rows(d$tests$altered_formulas, altered_formulas)
    attr(d$tests$altered_formulas, "test_name") <- "Altered Formulas"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}



#' @export
#' @rdname unPackDataChecks
checkDisaggs <- function(d, sheet, quiet = TRUE) {

  if (sheet %in% c("SNU x IM", "PSNUxIM")) {
    stop("Sorry! Can't check the PSNUxIM tab with this function.")
  }
  #TODO: Add functionality for PSNUxIM

  if (!quiet) {
    messages <- MessageQueue()
  }

  data <- unPackDataPackSheet(d,
                              sheet,
                              clean_orgs = TRUE,
                              clean_disaggs = FALSE,
                              clean_values = TRUE)

  valid_disaggs <- d$info$schema %>%
    dplyr::filter(
      sheet_name == sheet
      & (col_type == "target" | (col_type == "result" & dataset == "subnat"))) %>%
    dplyr::select(indicator_code, valid_ages, valid_sexes, valid_kps)

  defunct_disaggs <- data %>%
    dplyr::left_join(valid_disaggs, by = c("indicator_code" = "indicator_code")) %>%
    dplyr::filter(!purrr::map2_lgl(Age, valid_ages, ~.x %in% .y[["name"]])
                  | !purrr::map2_lgl(Sex, valid_sexes, ~.x %in% .y[["name"]])
                  | !purrr::map2_lgl(KeyPop, valid_kps, ~.x %in% .y[["name"]])) %>%
    dplyr::select(indicator_code, Age, Sex, KeyPop) %>%
    dplyr::distinct()

  if (NROW(defunct_disaggs) > 0) {

    defunct_msg <-
      utils::capture.output(
        print(as.data.frame(defunct_disaggs), row.names = FALSE))

    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ": INVALID DISAGGS. Please review all tabs flagged by this test to ensure",
        " no Age, Sex, or Key Population disaggregates have been inadvertently or",
        " incorrectly altered. If you believe this has been flagged in error, ",
        " please first refer to MER Guidance to confirm valid disaggregates for",
        " the data element flagged. (Check MER Guidance for correct alternatives.",
        " Also note that single-digit ages should be left-padded with zeros,",
        " e.g., 01-04 instead of 1-4.) -> \n\t",
        paste(defunct_msg, collapse = "\n\t"),
        "\n")

    d$tests$defunct_disaggs <- dplyr::bind_rows(d$tests$defunct_disaggs, defunct_disaggs)
    attr(d$tests$defunct_disaggs, "test_name") <- "Defunct disaggs"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}



#' @export
#' @rdname unPackDataChecks
checkSheetData <- function(d,
                           sheets = NULL,
                           quiet = TRUE,
                           ...) {

  interactive_print("Checking sheet data...")

  dots <- list(...)

  # Check/Fill in parameters ####
  params <- check_params(cop_year = d$info$cop_year,
                         tool = d$info$tool,
                         schema = d$info$schema,
                         sheets = sheets,
                         all_sheets = dots$all_sheets,
                         psnuxim = dots$psnuxim)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  # TODO: Apply method used in checkAnalytics line 631

  sheets <- sheets[!sheets %in% c("KP Validation")]

  for (sheet in sheets) {
    # Col Structure ----
    d <- checkColumnStructure(d, sheet, quiet) %>%

      # Duplicate Rows ----
      checkDupeRows(sheet, quiet) %>%

      # Non-numeric Values ----
      checkNonNumeric(sheet, quiet) %>%

      # Metadata ----
      # checkMissingMetadata(d, sheet, quiet) %>%
    # TODO: Remove this function. Covered by checkFormulas and checkDisaggs

    # Negative values ----
      checkNegativeValues(sheet, quiet) %>%

      # Decimal values ----
      checkDecimalValues(sheet, quiet) %>%

      # Check invalid org units ----
      checkInvalidOrgUnits(sheet, quiet) %>%

      # Check for invalid prioritizations ----
      checkInvalidPrioritizations(sheet, quiet) %>%

      # Formulas ----
      checkFormulas(sheet, quiet) %>%

      # TEST for defunct disaggs ####
      checkDisaggs(sheet, quiet)

    # TEST AGYW Tab for missing DSNUs ####
    # if (sheet == "AGYW") {
    #   DataPack_DSNUs <- d$data$extract %>%
    #     dplyr::select(PSNU, psnu_uid = psnuid) %>%
    #     dplyr::distinct() %>%
    #     dplyr::mutate(DataPack = 1)
    #
    #   DATIM_DSNUs <- datapackr::valid_PSNUs %>%
    #     dplyr::filter(country_uid %in% d$info$country_uids) %>%
    #     add_dp_psnu(.) %>%
    #     dplyr::arrange(dp_psnu) %>%
    #     dplyr::filter(!is.na(DREAMS)) %>%
    #     dplyr::select(PSNU = dp_psnu, psnu_uid, snu1) %>%
    #     dplyr::mutate(DATIM = 1)
    #
    #   DSNU_comparison <- DataPack_DSNUs %>%
    #     dplyr::full_join(DATIM_DSNUs, by = "psnu_uid")
    #
    #   d$tests$DSNU_comparison <- DSNU_comparison
    #   attr(d$tests$DSNU_comparison, "test_name") <- "DSNU List Comparison"
    #
    #   if (any(is.na(DSNU_comparison$DataPack))) {
    #     missing_DSNUs <- DSNU_comparison %>%
    #       dplyr::filter(is.na(DataPack))
    #
    #     msg <- paste0(
    #       "WARNING! In tab ",
    #       sheet,
    #       ": MISSING DREAMS SNUs found! ->  \n\t* ",
    #       paste(missing_DSNUs$PSNU.y, collapse = "\n\t* "),
    #       "\n")
    #
    #     d$info$messages <- appendMessage(d$info$messages, msg, "WARNING")
    #     d$info$missing_DSNUs <- TRUE
    #   }
    #
    #   if (any(is.na(DSNU_comparison$DATIM))) {
    #     invalid_DSNUs <- DSNU_comparison %>%
    #       dplyr::filter(is.na(DATIM))
    #
    #     msg <- paste0(
    #       "WARNING! In tab ",
    #       sheet,
    #       ": INVALID DREAMS SNUs found! ->  \n\t* ",
    #       paste(invalid_DSNUs$PSNU.x, collapse = "\n\t* "),
    #       "\n")
    #
    #     d$info$messages <- appendMessage(d$info$messages, msg, "WARNING")
    #   }
    #
    # }
  }

  return(d)

}
