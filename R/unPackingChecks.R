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
checkDupeRows <- function(sheets, d, quiet = TRUE) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  # Get header_cols
  header_cols <- d$info$schema %>%
    dplyr::filter(
      sheet_name %in% sheets,
      col_type == "row_header",
      !indicator_code %in% c("SNU1", "ID")) %>%
    dplyr::pull(indicator_code) %>%
    #c(., "mechCode_supportType") %>%
    unique()

  # Duplicates
  dupes <- purrr::map(d$sheets[sheets],
                      function(x) x %>%
                        dplyr::select(tidyselect::any_of(header_cols)) %>%
                        dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
                        dplyr::filter(!is.na(PSNU)) %>% # This is caught by checkInvalidOrgUnits
                        dplyr::filter(duplicated(.))) %>%
    purrr::keep(~ NROW(.x) > 0)

  if (length(dupes) > 0) {

    dupes %<>%
      purrr::map2(., names(.),
                  function(x, y) x %>%
                    dplyr::arrange(dplyr::across()) %>%
                    tibble::add_column(sheet = y, .before = 1))

    c$lvl <- "ERROR"

    c$msg <-
      purrr::map2(
        dupes, names(dupes),
        function(x, y)
          paste0(
            c$lvl, "! In tab ", y,
            ": DUPLICATE ROWS found. Ensure PSNUs or Age, Sex, KeyPop disaggregates",
            " are not repeated within tabs. This issue may have been caused by inadvertent",
            " or incorrect copying of data from one row to another. -> \n\t",
            paste(capture.output(print(as.data.frame(x), row.names = FALSE)),
                  collapse = "\n\t"),
            "\n"))

    c$result <- dplyr::bind_rows(dupes)
    attr(c$result, "test_name") <- "Duplicated rows"
    c$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()

      for (i in 1:length(c$msg)) {
        messages <- appendMessage(messages, c$msg[[i]], c$lvl)
      }

      printMessages(messages)
    }
  }

  return(c)
}


#' @export
#' @rdname unPackDataChecks
checkMissingCols <- function(sheets, d, quiet = TRUE) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  missing_cols <- d$sheets[sheets] %>%
    purrr::map2_dfr(
      .,
      names(.),
      function(x, y)
        d$info$schema %>%
          dplyr::filter(sheet_name == y,
                        !indicator_code %in% names(x)) %>%
          dplyr::select(sheet_name, indicator_code))

  # if (sheet == "PSNUxIM") {
  #   ## Drop all IM cols (left & right sides)
  #   schema_cols %<>%
  #     dplyr::filter(
  #       col_type != "allocation",
  #       !(col_type == "target"
  #         & (indicator_code %in% c("Not PEPFAR", "12345_DSD", ""))))
  #
  #   ## We don't care to track col issues with blank/NA cols in PSNUxIM
  #   submission_cols %<>%
  #     dplyr::filter(!is.na(indicator_code),
  #                   !indicator_code %in% c("")) %>%
  #     ## Standardize mech/support_type names
  #     dplyr::mutate(
  #       indicator_code =
  #         dplyr::case_when(
  #           stringr::str_detect(indicator_code, "\\d+")
  #           ~ paste0(stringr::str_extract(indicator_code, "\\d+"),
  #                    "_",
  #                    stringr::str_extract(indicator_code, "DSD|TA")),
  #           TRUE ~ indicator_code))
  # }

  if (NROW(missing_cols) > 0) {

    c$lvl <- "WARNING"

    c$msg <- unique(missing_cols$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x)
          paste0(
            c$lvl, "! In tab ", x,
            ", MISSING COLUMNS: Please ensure no columns have been deleted or renamed from",
            " the original Data Pack you have received. ->  \n\t* ",
            paste(missing_cols$indicator_code[missing_cols$sheet_name == x],
                  collapse = "\n\t* "),
            "\n"))

    c$result <- missing_cols
    attr(c$result, "test_name") <- "Missing columns"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in 1:length(c$msg)) {
        messages <- appendMessage(messages, c$msg[[i]], c$lvl)
      }
      printMessages(messages)
    }
  }

  return(c)

}


#' @export
#' @rdname unPackDataChecks
checkDupeCols <- function(sheets, d, quiet = TRUE) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  dup_cols <- d$sheets[sheets] %>%
    purrr::map2_dfr(
      .,
      names(.),
      function(x, y)
        tibble::enframe(names(x), name = NULL, value = "indicator_code") %>%
        dplyr::filter(duplicated(.)) %>%
        dplyr::distinct() %>%
        tibble::add_column(sheet = y, .before = 1)) %>%
    dplyr::left_join(
      d$info$schema %>%
        dplyr::filter(sheet_name %in% sheets) %>%
        dplyr::mutate(critical = !is.na(col)) %>%
        dplyr::select(sheet = sheet_name, indicator_code, critical),
      by = c("sheet", "indicator_code"))

  # if (sheet == "PSNUxIM") {
  #   ## Drop all IM cols (left & right sides)
  #   schema_cols %<>%
  #     dplyr::filter(
  #       col_type != "allocation",
  #       !(col_type == "target"
  #         & (indicator_code %in% c("Not PEPFAR", "12345_DSD", ""))))
  #
  #   ## We don't care to track col issues with blank/NA cols in PSNUxIM
  #   submission_cols %<>%
  #     dplyr::filter(!is.na(indicator_code),
  #                   !indicator_code %in% c("")) %>%
  #     ## Standardize mech/support_type names
  #     dplyr::mutate(
  #       indicator_code =
  #         dplyr::case_when(
  #           stringr::str_detect(indicator_code, "\\d+")
  #           ~ paste0(stringr::str_extract(indicator_code, "\\d+"),
  #                    "_",
  #                    stringr::str_extract(indicator_code, "DSD|TA")),
  #           TRUE ~ indicator_code))
  # }

  # if (sheet == "PSNUxIM") {
  #   last_im_int <- dup_cols %>%
  #     dplyr::filter(critical) %>%
  #     dplyr::mutate(
  #       jump = submission_order - dplyr::lag(submission_order, default = 0)) %>%
  #     dplyr::filter(jump > 1) %>%
  #     dplyr::pull(submission_order) %>%
  #     min() - 1
  #
  #   # first_im_int <- max(dup_cols$template_order, na.rm = T)
  #   # b4_im <- dup_cols$submission_order[which(dup_cols$template_order == first_im_int)]
  #
  #   dup_cols %<>%
  #     dplyr::mutate(
  #       grp =
  #         dplyr::case_when(
  #           critical ~ "Critical",
  #           submission_order < last_im_int ~ "IM Allocations",
  #           TRUE ~ "IM Targets"))
  # }

  if (NROW(dup_cols) > 0) {

    c$lvl <- "ERROR"

    c$msg <- unique(dup_cols$sheet) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x)
          paste0(
            c$lvl, "! In tab ", x,
            ", DUPLICATE COLUMNS: The following columns appear multiple times. This",
            " must be resolved in your submission, especially for those columns",
            " noted as [Critical!]. ->  \n\t* ",
            paste(
              dup_cols[dup_cols$sheet == x, ] %>%
                dplyr::mutate(
                  msg_col = paste0(indicator_code,
                                   ifelse(!is.na(critical), " [Critical!]", ""))) %>%
                dplyr::pull(msg_col),
              collapse = "\n\t* "),
            "\n"))

    c$result <- dup_cols
    attr(c$result, "test_name") <- "Duplicate columns"
    c$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in 1:length(c$msg)) {
        messages <- appendMessage(messages, c$msg[[i]], c$lvl)
      }
      printMessages(messages)
    }
  }

  return(c)

}

#' @export
#' @rdname unPackDataChecks
checkOutOfOrderCols <- function(sheets, d, quiet = TRUE) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  out_of_order <- d$sheets[sheets] %>%
    purrr::map2_dfr(
      .,
      names(.),
      function(x, y)
        names(x) %>%
        tibble::enframe(name = "submission_order", value = "indicator_code") %>%
        tibble::add_column(sheet = y, .before = 1)) %>%
      dplyr::right_join(
        d$info$schema %>%
          dplyr::filter(sheet_name %in% sheets) %>%
          dplyr::select(indicator_code, sheet = sheet_name, template_order = col),
        by = c("indicator_code", "sheet")) %>%
    dplyr::filter(submission_order != template_order)

  # if (sheet == "PSNUxIM") {
  #   ## Drop all IM cols (left & right sides)
  #   schema_cols %<>%
  #     dplyr::filter(
  #       col_type != "allocation",
  #       !(col_type == "target"
  #         & (indicator_code %in% c("Not PEPFAR", "12345_DSD", ""))))
  #
  #   ## We don't care to track col issues with blank/NA cols in PSNUxIM
  #   submission_cols %<>%
  #     dplyr::filter(!is.na(indicator_code),
  #                   !indicator_code %in% c("")) %>%
  #     ## Standardize mech/support_type names
  #     dplyr::mutate(
  #       indicator_code =
  #         dplyr::case_when(
  #           stringr::str_detect(indicator_code, "\\d+")
  #           ~ paste0(stringr::str_extract(indicator_code, "\\d+"),
  #                    "_",
  #                    stringr::str_extract(indicator_code, "DSD|TA")),
  #           TRUE ~ indicator_code))
  # }

  if (NROW(out_of_order) > 0) {

    c$lvl <- "WARNING"

    c$msg <- unique(out_of_order$sheet) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x)
          paste0(
            c$lvl, "! In tab ", x,
            ", OUT OF ORDER COLUMNS: While it is permitted to rearrange columns",
            " within your Data Pack as needed, this is not encouraged as it may",
            " introduce unintended formula errors. Please review these columns to",
            " ensure their rearrangement has not caused any issues. -> \n\t* ",
            paste(unique(out_of_order$indicator_code[out_of_order$sheet == x]),
                  collapse = "\n\t* "),
            "\n"))

    c$result <- out_of_order
    attr(c$result, "test_name") <- "Columns out of order"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in 1:length(c$msg)) {
        messages <- appendMessage(messages, c$msg[[i]], c$lvl)
      }
      printMessages(messages)
    }
  }

  # TODO: Add PSNUxIM check for malformed IM/type headers
  # TODO: Add PSNUxIM check for making sure IM appears once in both L & R

  return(c)

}


#' @export
#' @rdname unPackDataChecks
checkNonNumeric <- function(sheets, d, quiet = TRUE) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  non_numeric <- unPackDataPackSheet(d,
                                     sheets,
                                     clean_orgs = F,
                                     clean_disaggs = F,
                                     clean_values = F) %>%
    dplyr::filter(
      !(sheet_name == "Prioritization"
        & stringr::str_sub(PSNU, 1, 9) == "_Military")) %>%
    tidyr::drop_na(value)

  non_numeric <-
    non_numeric[which(is.na(suppressWarnings(as.numeric(non_numeric$value)))), ]

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

  if (NROW(non_numeric) > 0) {

    c$lvl <- "WARNING"

    c$msg <- unique(non_numeric$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x)
          paste0(
            c$lvl, "! In tab ", x,
            ": NON-NUMERIC VALUES found! Please check the following columns for",
            " possible non-numeric values. ->  \n\t* ",
            paste(
              unique(
                non_numeric$indicator_code[non_numeric$sheet_name == x]),
              collapse = "\n\t* "),
            "\n"))

    c$result <- non_numeric
    attr(c$result, "test_name") <- "Non-numeric values"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in 1:length(c$msg)) {
        messages <- appendMessage(messages, c$msg[[i]], c$lvl)
      }
      printMessages(messages)
    }
  }

  return(c)
}


#' @export
#' @rdname unPackDataChecks
checkNegativeValues <- function(sheets, d, quiet = T) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  negative_values <- unPackDataPackSheet(d,
                                         sheets,
                                         clean_orgs = F,
                                         clean_disaggs = F,
                                         clean_values = F) %>%
    dplyr::filter(value < 0)

  if (NROW(negative_values) > 0) {

    c$lvl <- "ERROR"

    c$msg <- unique(negative_values$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x)
          paste0(
            c$lvl, "! In tab ", x,
            ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
            " against Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
            paste(
              unique(
                negative_values$indicator_code[negative_values$sheet_name == x]),
              collapse = "\n\t* "),
            "\n"))

    c$result <- negative_values
    attr(c$result, "test_name") <- "Negative values"
    c$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in 1:length(c$msg)) {
        messages <- appendMessage(messages, c$msg[[i]], c$lvl)
      }
      printMessages(messages)
    }
  }

  return(c)
}



#' @export
#' @rdname unPackDataChecks
checkDecimalValues <- function(sheets, d, quiet = TRUE) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  decimals_allowed <- d$info$schema %>%
    dplyr::filter(sheet_name %in% sheets,
                  col_type == "target",
                  value_type == "percentage") %>%
    dplyr::pull(indicator_code)

  decimal_cols <- unPackDataPackSheet(d,
                                      sheets,
                                      clean_orgs = F,
                                      clean_disaggs = F,
                                      clean_values = F) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(value %% 1 != 0
                  & !indicator_code %in% decimals_allowed)

  if (NROW(decimal_cols) > 0) {
    c$lvl <- "WARNING"

    c$msg <- unique(decimal_cols$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x)
          paste0(
            c$lvl,
            "! In tab ", x,
            ": DECIMAL VALUES found in the following columns that should have only",
            " whole, positive, numeric values. These will be rounded. -> \n\t* ",
            paste(
              unique(
                decimal_cols$indicator_code[decimal_cols$sheet_name == x]),
              collapse = "\n\t* "),
            "\n"))

    c$result <- decimal_cols
    attr(c$result, "test_name") <- "Decimal values"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in 1:length(c$msg)) {
        messages <- appendMessage(messages, c$msg[[i]], c$lvl)
      }
      printMessages(messages)
    }
  }

  return(c)
}


#' @export
#' @rdname unPackDataChecks
checkInvalidOrgUnits <- function(sheets, d, quiet = TRUE) {

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  invalid_orgunits <- d$sheets[sheets] %>%
    purrr::map_dfr(`[`, "PSNU") %>%
    dplyr::bind_rows(.id = "sheet_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate(psnuid = extract_uid(PSNU)) %>%
    dplyr::anti_join(valid_PSNUs, by = c("psnuid" = "psnu_uid"))

  na_orgunits <- invalid_orgunits[is.na(invalid_orgunits$PSNU), ]

  if (NROW(invalid_orgunits) > 0 | (NROW(na_orgunits) > 0 & NROW(data) > 0)) {

    c$lvl <- "ERROR"

    c$msg <- unique(decimal_cols$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x)
      paste0(
        c$lvl, "! In tab ", sheet,
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

    c$result <- invalid_orgunits
    attr(c$result, "test_name") <- "Invalid orgunits"
    #d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    c$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, c$msg, c$lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(c)
}


#' @export
#' @rdname unPackDataChecks
checkInvalidPrioritizations <- function(sheet, d, quiet = T) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE,
            sheet = sheet)

  if (!sheet == "Prioritization") {
    return(c)
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

    c$lvl <- "ERROR"

    c$msg <-
      paste0(
        c$lvl, "! In tab ",
        sheet,
        ": INVALID PRIORITIZATIONS: The following PSNUs have been assigned",
        " invalid or blank prioritizations. Please note that all PSNUs must have",
        " an assigned prioritization, and prioritizations can only be assigned ",
        paste_oxford(prioritization_dict()$value, final = "or"), ". -> \n\t",
        paste(inv_pzs_msg, collapse = "\n\t"),
        "\n")

    c$result <- invalid_prioritizations
    attr(c$result, "test_name") <- "Invalid prioritizations"
    #d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    c$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, c$msg, c$lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(c)
}



#' @export
#' @rdname unPackDataChecks
checkFormulas <- function(sheet, d, quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE,
            sheet = sheet)

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

    c$lvl <- "WARNING"

    cols_affected <- altered_formulas %>%
      dplyr::select(indicator_code, correct_fx, count) %>%
      dplyr::group_by(indicator_code, correct_fx) %>%
      dplyr::summarize(count = sum(count)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(fx_violations = paste0(indicator_code, ":  ", count))

    c$msg <-
      paste0(
        c$lvl, "! In tab ",
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

    c$result <- altered_formulas
    attr(c$result, "test_name") <- "Altered Formulas"
    #d$info$messages <- appendMessage(d$info$messages, msg, lvl)

    if (!quiet) {
      messages <- appendMessage(messages, c$msg, c$lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(c)
}



#' @export
#' @rdname unPackDataChecks
checkDisaggs <- function(sheet, d, quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  if (sheet %in% c("SNU x IM", "PSNUxIM")) {
    stop("Sorry! Can't check the PSNUxIM tab with this function.")
  }
  #TODO: Add functionality for PSNUxIM

  c <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE,
            sheet = sheet)

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

    c$lvl <- "ERROR"

    c$msg <-
      paste0(
        c$lvl, "! In tab ",
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

    c$result <- defunct_disaggs
    attr(c$result, "test_name") <- "Defunct disaggs"
    #d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    c$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, c$msg, c$lvl)
    }
  }

  if (!quiet) {
    printMessages(messages)
  }

  return(c)
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

  sheets <- sheets[!sheets %in% c("KP Validation")]

  # Apply the list of check functions ----
  funs <- list(
    duplicate_rows = checkDupeRows,
    missing_cols = checkMissingCols,
    duplicate_columns = checkDupeCols,
    columns_out_of_order = checkOutOfOrderCols,
    non_numeric = checkNonNumeric,
    negative_values  = checkNegativeValues,
    decimal_values = checkDecimalValues,
    invalid_orgunits = checkInvalidOrgUnits,
    invalid_prioritizations = checkInvalidPrioritizations,
    altered_formulas = checkFormulas,
    defunct_disaggs = checkDisaggs
  )

  sheet_checks <- function(.f, sheets, d) {

    r <- purrr::map(sheets, .f, d)

    result <- purrr::map_dfr(r, function(x) purrr::pluck(x, "result"))
    msg <- purrr::map_chr(r, function(x) purrr::pluck(x, "msg") %||% NA_character_)
    msg <- msg[!is.na(msg)]
    lvl <- purrr::map_chr(r, function(x) purrr::pluck(x, "lvl") %||% NA_character_)
    lvl <- lvl[!is.na(lvl)]
    has_error <- purrr::map_lgl(r, function(x) purrr::pluck(x, "has_error") %||% NA)
    has_error <- any(has_error)

    s <- list(result = result,
              msg = msg,
              lvl = lvl,
              has_error = has_error)

    return(s)
  }

  # TODO: Make sure all functions note sheet name as column in result
  # TODO: Make sure all functions row bind results

  data_checks <- purrr::map(funs, sheet_checks, sheets, d)

  d$tests <-
    append(d$tests,
           purrr::map(data_checks,
                      function(x) purrr::pluck(x, "result"))) %>%
    purrr::discard(is.null)

  msg <- purrr::map(data_checks, function(x) purrr::pluck(x, "msg")) %>%
    Reduce(f = c, x = .)
  lvl <- purrr::map(data_checks, function(x) purrr::pluck(x, "lvl")) %>%
    Reduce(f = c, x = .)

  for (i in 1:length(msg)) {
    d$info$messages <- appendMessage(d$info$messages, msg[i], lvl[i])
  }

  d$info$has_error <-
    purrr::map_lgl(data_checks, function(x) purrr::pluck(x, "has_error")) %>%
    c(., d$info$has_error) %>%
    any()





    # TODO: TEST AGYW Tab for missing DSNUs ####
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

  return(d)

}
