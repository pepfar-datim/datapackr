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
#' `checkToolEmptySheets` detects whether a sheet is essentially empty (no data
#'   in rows, or no data in row 14 column headers).
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
#' @inheritParams datapackr_params
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

  if (d$info$tool == "PSNUxIM") {
    schema_sheets <- schema_sheets[schema_sheets != "Spectrum"]
  }

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

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  #This test requires index columns. If they are not there, drop the sheets
  #If they are missing index columns, we are not going to
  #Attempt to process them
  if (!is.null(d$tests$missing_index_columns)) {
    sheets <- sheets[!(sheets %in% d$tests$missing_index_columns$sheet_name)]
  }

  if (length(sheets) == 0) {
    return(ch)
  }

  # Get header_cols
  header_cols <- purrr::map(sheets, function(x) {
    d$info$schema %>%
      dplyr::filter(
        sheet_name %in% x,
        col_type == "row_header",
        !indicator_code %in% c("SNU1", "ID")) %>%
      dplyr::pull(indicator_code) %>%
      #c(., "mechCode_supportType") %>% # DP-472
      unique()
  })

  # Duplicates
  dupes <- purrr::map2(d$sheets[sheets], header_cols,
                      function(x, y) {
                        x %>%
                          dplyr::select(tidyselect::all_of(y)) %>%
                          dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
                          dplyr::filter(!is.na(PSNU)) %>% # This is caught by checkInvalidOrgUnits
                          dplyr::filter(duplicated(.))
                      }) %>%
    purrr::keep(~ NROW(.x) > 0)


  if (length(dupes) > 0) {

    dupes %<>%
      purrr::map2(., names(.),
                  function(x, y) {
                    x %>%
                      dplyr::arrange(dplyr::across(tidyselect::everything())) %>%
                      tibble::add_column(sheet = y, .before = 1)
                  })

    ch$lvl <- "ERROR"

    ch$msg <-
      purrr::map2(
        dupes, names(dupes),
        function(x, y) {
          paste0(
            ch$lvl, "! In tab ", y,
            ": DUPLICATE ROWS found. Ensure PSNUs or Age, Sex, KeyPop disaggregates",
            " are not repeated within tabs. This issue may have been caused by inadvertent",
            " or incorrect copying of data from one row to another. -> \n\t",
            paste(capture.output(print(as.data.frame(x), row.names = FALSE)),
                  collapse = "\n\t"),
            "\n")
        })

    ch$result <- dplyr::bind_rows(dupes)
    attr(ch$result, "test_name") <- "Duplicated rows"
    ch$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()

      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }

      printMessages(messages)
    }
  }

  return(ch)
}


#' @export
#' @rdname unPackDataChecks
checkMissingCols <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  missing_cols <- d$sheets[sheets] %>%
    purrr::map2_dfr(
      .,
      names(.),
      function(x, y) {
        d$info$schema %>%
          dplyr::filter(sheet_name == y,
                        !indicator_code %in% names(x)) %>%
          dplyr::select(sheet_name, indicator_code)
      })

  # if (sheet == "PSNUxIM") { # DP-472
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

    ch$lvl <- "ERROR"

    ch$msg <- unique(missing_cols$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x,
            ", MISSING COLUMNS: Please ensure no columns have been deleted or renamed from",
            " the original Data Pack you have received. ->  \n\t* ",
            paste(missing_cols$indicator_code[missing_cols$sheet_name == x],
                  collapse = "\n\t* "),
            "\n")
        })

    ch$result <- missing_cols
    attr(ch$result, "test_name") <- "Missing columns"
    ch$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)

}


#' @export
#' @rdname unPackDataChecks
checkDupeCols <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  dup_cols <- d$sheets[sheets] %>%
    purrr::map2_dfr(
      .,
      names(.),
      function(x, y) {
        tibble::enframe(names(x), name = NULL, value = "indicator_code") %>%
          dplyr::filter(duplicated(.)) %>%
          dplyr::distinct() %>%
          tibble::add_column(sheet = y, .before = 1)
      }) %>%
    dplyr::left_join(
      d$info$schema %>%
        dplyr::filter(sheet_name %in% sheets) %>%
        dplyr::mutate(critical = !is.na(col)) %>%
        dplyr::select(sheet = sheet_name, indicator_code, critical),
      by = c("sheet", "indicator_code"))

  # if (sheet == "PSNUxIM") { # DP-472
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

  # if (sheet == "PSNUxIM") { # DP-472
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

    ch$lvl <- "ERROR"

    ch$msg <- unique(dup_cols$sheet) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x,
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
            "\n")
        })

    ch$result <- dup_cols
    attr(ch$result, "test_name") <- "Duplicate columns"
    ch$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)

}

#' @export
#' @rdname unPackDataChecks
checkOutOfOrderCols <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  out_of_order <- d$sheets[names(d$sheets) %in% sheets] %>%
    purrr::map2_dfr(
      .,
      names(.),
      function(x, y) {
        names(x) %>%
          tibble::enframe(name = "submission_order", value = "indicator_code") %>%
          tibble::add_column(sheet = y, .before = 1)
      }) %>%
    dplyr::right_join(
      d$info$schema %>%
        dplyr::filter(sheet_name %in% sheets) %>%
        dplyr::select(indicator_code, sheet = sheet_name, template_order = col),
      by = c("indicator_code", "sheet")) %>%
    dplyr::filter(submission_order != template_order)

  # if (sheet == "PSNUxIM") { # DP-472
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

    ch$lvl <- "WARNING"

    ch$msg <- unique(out_of_order$sheet) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x,
            ", OUT OF ORDER COLUMNS: While it is permitted to rearrange columns",
            " within your Data Pack as needed, this is not encouraged as it may",
            " introduce unintended formula errors. Please review these columns to",
            " ensure their rearrangement has not caused any issues. -> \n\t* ",
            paste(unique(out_of_order$indicator_code[out_of_order$sheet == x]),
                  collapse = "\n\t* "),
            "\n")
        })

    ch$result <- out_of_order
    attr(ch$result, "test_name") <- "Columns out of order"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)

}


#' @export
#' @rdname unPackDataChecks
checkNonNumeric <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  non_numeric <- unPackDataPackSheet(d,
                                     sheets,
                                     clean_orgs = FALSE,
                                     clean_disaggs = FALSE,
                                     clean_values = FALSE) %>%
    dplyr::filter(
      !(sheet_name == "Prioritization"
        & stringr::str_sub(PSNU, 1, 9) == "_Military")) %>%
    tidyr::drop_na(value)

  non_numeric <-
    non_numeric[which(is.na(suppressWarnings(as.numeric(non_numeric$value)))), ]

  if (NROW(non_numeric) > 0) {

    ch$lvl <- "WARNING"

    ch$msg <- unique(non_numeric$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x,
            ": NON-NUMERIC VALUES found! Please check the following columns for",
            " possible non-numeric values. ->  \n\t* ",
            paste(
              unique(
                non_numeric$indicator_code[non_numeric$sheet_name == x]),
              collapse = "\n\t* "),
            "\n")
        })

    ch$result <- non_numeric
    attr(ch$result, "test_name") <- "Non-numeric values"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)
}


#' @export
#' @rdname unPackDataChecks
checkNegativeValues <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  negative_values <- unPackDataPackSheet(d,
                                         sheets,
                                         clean_orgs = FALSE,
                                         clean_disaggs = FALSE,
                                         clean_values = FALSE) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(value < 0)

  if (NROW(negative_values) > 0) {

    ch$lvl <- "ERROR"

    ch$msg <- unique(negative_values$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x,
            ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
            " against Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
            paste(
              unique(
                negative_values$indicator_code[negative_values$sheet_name == x]),
              collapse = "\n\t* "),
            "\n")
        })

    ch$result <- negative_values
    attr(ch$result, "test_name") <- "Negative values"
    ch$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)
}



#' @export
#' @rdname unPackDataChecks
checkDecimalValues <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
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
                                      clean_orgs = FALSE,
                                      clean_disaggs = FALSE,
                                      clean_values = FALSE) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(value %% 1 != 0
                  & !indicator_code %in% decimals_allowed)

  if (NROW(decimal_cols) > 0) {
    ch$lvl <- "WARNING"

    ch$msg <- unique(decimal_cols$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl,
            "! In tab ", x,
            ": DECIMAL VALUES found in the following columns that should have only",
            " whole, positive, numeric values. These will be rounded. -> \n\t* ",
            paste(
              unique(
                decimal_cols$indicator_code[decimal_cols$sheet_name == x]),
              collapse = "\n\t* "),
            "\n")
        })

    ch$result <- decimal_cols
    attr(ch$result, "test_name") <- "Decimal values"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)
}


#' @export
#' @rdname unPackDataChecks
checkInvalidOrgUnits <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  valid_orgunits_local <- getValidOrgUnits(d$info$cop_year)

  #There may be some variation in the columns between cop years
  cols_to_filter <- switch(as.character(d$info$cop_year),
                           "2023" = c("PSNU", "Age", "Sex"),
                           "2024" = c("PSNU", "Age", "Sex"))

  invalid_orgunits <- d$sheets[sheets] %>%
    dplyr::bind_rows(.id = "sheet_name") %>%
    #Reverting this back to the previous logic to filter
    #to ignore any rows which are NA in the columsn to filter.
    dplyr::filter(dplyr::if_any(tidyselect::any_of(cols_to_filter), ~ !is.na(.))) %>%
    #dplyr::filter(dplyr::if_all(tidyselect::all_of(cols_to_filter), ~ !is.na(.x))) %>%
    dplyr::select(sheet_name, PSNU) %>%
    dplyr::distinct() %>%
    dplyr::mutate(snu_uid = extract_uid(PSNU)) %>%
    dplyr::anti_join(valid_orgunits_local, by = c("snu_uid" = "uid"))

  na_orgunits <- invalid_orgunits[is.na(invalid_orgunits$PSNU), ]

  if (NROW(invalid_orgunits) > 0 || NROW(na_orgunits) > 0) {

    ch$lvl <- "ERROR"

    ch$msg <- unique(invalid_orgunits$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x,
            ", INVALID OR BLANK ORG UNITS: ",
            ifelse(
              NROW(na_orgunits) > 0,
              paste0("There are ", NROW(na_orgunits),
                     " rows where PSNU/DSNU was left blank."),
              ""),
            " Please also review the below PSNUs/DSNUs with invalid or missing org",
            " unit UIDs. (This is an 11-digit alphanumeric code assigned in DATIM to",
            " each organization unit.) If you believe these are valid, confirm in",
            " both DATIM & FACTSInfo that the below are correctly added and active",
            " for the appropriate COP Year. ->  \n\t* ",
            paste(invalid_orgunits$PSNU[!is.na(invalid_orgunits$PSNU)], collapse = "\n\t* "),
            "\n")
        })

    ch$result <- invalid_orgunits
    attr(ch$result, "test_name") <- "Invalid orgunits"
    ch$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)
}


#' @export
#' @rdname unPackDataChecks
checkInvalidPrioritizations <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  valid_orgunits_local <- getValidOrgUnits(d$info$cop_year)
  valid_orgunits_local$hierarchy_level  <- unlist(lapply(valid_orgunits_local$ancestors, function(x) NROW(x) + 1L))
  valid_orgunits_local <- valid_orgunits_local[, c("uid", "ou_uid", "country_uid", "hierarchy_level")]


  data <- d$sheets[["Prioritization"]][, c("PSNU", "IMPATT.PRIORITY_SNU.T")]
  names(data)[names(data) == "IMPATT.PRIORITY_SNU.T"] <- "value"
  data <- data[, c("PSNU", "value")]
  data$snu_uid <- extract_uid(data$PSNU)

  data %<>% dplyr::left_join(valid_orgunits_local, by = c("snu_uid" = "uid"))

  dataset_levels_local <- datapackr::dataset_levels %>%
    dplyr::filter(cop_year == d$info$cop_year, ou_uid == d$info$operating_unit$ou_uid) %>%
    dplyr::select(ou_uid, country_uid, prioritization)

  data %<>% dplyr::left_join(dataset_levels_local)

  #
  data <- data %>%
    dplyr::mutate(
      isInvalidPSNU = dplyr::case_when(
        is.na(ou_uid) | is.na(country_uid) ~ TRUE,
        grepl("_Military", PSNU) ~ FALSE,
        hierarchy_level == prioritization ~ FALSE,
        TRUE ~ TRUE
      )
    )

  isInvalidPrioritization <- function(PSNU, value) {

    if (grepl("_Military", PSNU)) {
       value != "M"
    } else {
      !(value %in% prioritization_dict()$value)
    }

  }

  data$isInvalidPrioritization <- mapply(isInvalidPrioritization, data$PSNU, data$value)

  invalid_prioritizations <- data[data$isInvalidPSNU | data$isInvalidPrioritization, ]

  if (NROW(invalid_prioritizations) > 0) {

    inv_pzs_msg <-
      utils::capture.output(
        print(as.data.frame(invalid_prioritizations), row.names = FALSE))

    ch$lvl <- "ERROR"

    ch$msg <-
      paste0(
        ch$lvl, "! In tab Prioritization",
        ": INVALID PRIORITIZATIONS: The following PSNUs have been assigned",
        " invalid or blank prioritizations. Please note that all PSNUs must have",
        " an assigned prioritization, and prioritizations can only be assigned ",
        paste_oxford(prioritization_dict()$value, final = "or"), ". -> \n\t",
        paste(inv_pzs_msg, collapse = "\n\t"),
        "\n")

    ch$result <- invalid_prioritizations
    attr(ch$result, "test_name") <- "Invalid prioritizations"
    ch$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)
}


#Extracts grey cells from Row3 for all sheets
getCriticalColumns <- function()  {

  template_file <- system.file("extdata/COP23_Data_Pack_Template.xlsx", package = "datapackr")

  template <- readxl::read_excel(template_file)
  cells <- tidyxl::xlsx_cells(template_file)
  formats <- formats <- tidyxl::xlsx_formats(template_file)

  grey_cells <- which(formats$local$fill$patternFill$fgColor$rgb == "FFFFFFFF")
  critical_cols <- cells$local_format_id %in% grey_cells

  critical_columns <- cells[critical_cols, ] %>%
    dplyr::filter(row == 3)  %>%
    dplyr::select(sheet_name = sheet, col) %>%
    dplyr::mutate(critical = "Y")

  critical_columns
}


#' @export
#' @rdname unPackDataChecks
checkFormulas <- function(sheets, d, quiet = TRUE) {

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

  # Pull in formulas from schema
  formulas_schema <- d$info$schema %>%
    dplyr::filter(
      sheet_name %in% sheets,
      !is.na(formula)) %>%
    dplyr::mutate(
      formula = stringr::str_replace_all(formula,
                                         "(?<=[:upper:])\\d+",
                                         "\\\\d+"))

  if (d$info$cop_year >= "2023") {

    critical_columns <- getCriticalColumns()

    formulas_schema <- formulas_schema %>%
      dplyr::left_join(critical_columns, by = c("sheet_name", "col")) %>%
      dplyr::mutate(critical = dplyr::case_when(is.na(critical) ~ "N",
                                                TRUE ~ critical)) %>%
      dplyr::select(-col)

  }

  formulas_schema %<>% dplyr::select(sheet_num, sheet_name, indicator_code, fx_schema = formula,
                                     critical)
  # Pull in formulas from Data Pack sheet
  formulas_datapack <-
    tidyxl::xlsx_cells(path = d$keychain$submission_path,
                       sheets = sheets,
                       include_blank_cells = TRUE)

  #By default, tidyxl scans forward when include_blank_cells
  #is true is TRUE.
  #Lets try and define where the last row of data is and exclude these blank

  last_row_of_data <- formulas_datapack %>%
    dplyr::filter(data_type != "blank") %>%
    dplyr::pull(row) %>%
    max()

  formulas_datapack %<>%
    dplyr::filter(row <= last_row_of_data) %>%
    # Note that this function won't pick up any cols with blank indicator_code
    dplyr::filter(row >= header_row) %>%
    dplyr::mutate(formula = dplyr::if_else(is.na(formula),
                                           as.character(numeric),
                                           formula),
                  formula = dplyr::if_else(is.na(formula), character, formula)) %>%
    dplyr::select(sheet_name = sheet, row, col, character, formula)

  indicator_codes <- formulas_datapack %>%
    dplyr::filter(row == header_row, !is.na(character)) %>%
    dplyr::select(sheet_name, col, indicator_code = character) %>%
    dplyr::semi_join(formulas_schema, by = c("indicator_code", "sheet_name"))

  formulas_datapack %<>%
    dplyr::left_join(indicator_codes, by = c("col", "sheet_name")) %>%
    dplyr::select(sheet_name, row, indicator_code, formula) %>%
    dplyr::filter(row != header_row,
                  !is.na(indicator_code)) %>%
    # purrr::when( # DP-472
    #   sheet == "PSNUxIM" & d$info$tool == "Data Pack" ~ .,
    #   ~  dplyr::group_by(., row) %>%
    #     dplyr::mutate(occurrence = duplicated(indicator_code)) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::filter(occurrence == FALSE) %>%
    #     dplyr::select(-occurrence) %>%
    #     # Limit to only columns that DUIT cares about
    #     dplyr::filter(indicator_code %in% formulas_schema$indicator_code)
    # ) %>%
    dplyr::mutate(
      formula = stringr::str_replace_all(formula,
                                         "(?<=[:upper:])\\d+",
                                         "\\\\d+"))

  # Compare formulas from schema against Data Pack to see diffs
  altered_formulas <- formulas_datapack %>%
    dplyr::anti_join(formulas_schema,
                     by = c("sheet_name" = "sheet_name",
                            "indicator_code" = "indicator_code",
                            "formula" = "fx_schema")) %>%
    dplyr::left_join(formulas_schema,
                     by = c("sheet_name", "indicator_code")) %>%
    # dplyr::left_join( # DP-472
    #   formulas_schema,
    #   by = ifelse(sheet == "PSNUxIM" & d$info$tool == "Data Pack",
    #               c("col" = "col"),
    #               c("indicator_code" = "indicator_code"))) %>%
    # purrr::when(sheet == "PSNUxIM" & d$info$tool == "Data Pack" ~ dplyr::rename(., indicator_code = indicator_code.y),
    #             ~ .) %>%
    dplyr::select(sheet_num, sheet_name, row, indicator_code,
                  correct_fx = fx_schema, submitted_fx = formula, critical) %>%
    dplyr::filter(critical == "Y") #Ignore non-critical formulas

  if (NROW(altered_formulas) > 0) {

    ch$lvl <- "WARNING"

    cols_affected <- altered_formulas %>%
      dplyr::count(sheet_num, sheet_name, indicator_code, critical, name = "count") %>%
      dplyr::mutate(fx_violations = paste0(indicator_code, ":  ", count))

    critical <- cols_affected[cols_affected$critical == "Y", ]

    ch$msg <- unique(cols_affected$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x, ", ",
            sum(critical$count[critical$sheet_name == x]),
            " CRITICAL ALTERED FORMULAS",
            " Altering formulas in Grey colored columns without DUIT and PPM",
            " approval may lead to programmatic and technical issues in your Data ",
            " Pack. This warning may be triggered by deleting or overwriting a",
            " formula, or a manual fix not being applied. See the provided",
            " Validation Results file for detail on both critical and",
            " non-critical formulas",
            ifelse(
              NROW(critical[critical$sheet_name == x, ]) > 0,
              paste0(", and below for the number of violations",
                     " against critical columns ->  \n\t* ",
                     paste(critical$fx_violations[critical$sheet_name == x],
                           collapse = "\n\t* ")),
              "."),
            "\n")
        })

    altered_formulas %<>%
      dplyr::group_by(sheet_num, sheet_name, indicator_code, correct_fx,
                      submitted_fx, critical) %>%
      dplyr::summarise(affected_rows = formatSetStrings(row),
                       .groups = "drop") %>%
      dplyr::ungroup()

    # Alternative with less detail but more manageably sized: (Keep and check with DUIT)
    # altered_formulas %<>%
    #   dplyr::group_by(sheet_num, sheet_name, indicator_code, correct_fx,
    #                   critical) %>%
    #   dplyr::summarise(affected_rows = formatSetStrings(row),
    #                    .groups = "drop") %>%
    #   dplyr::ungroup()

    ch$result <- altered_formulas
    attr(ch$result, "test_name") <- "Altered Formulas"

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)
}


#' @export
#' @rdname unPackDataChecks
checkDisaggs <- function(sheets, d, quiet = TRUE) {

  if (any(c("SNU x IM", "PSNUxIM") %in% sheets)) {
    interactive_warning("Sorry! Can't check the PSNUxIM tab with this function.") # DP-472
  }
  sheets <- sheets[sheets != "PSNUxIM"]

  ch <- list(result = NULL,
            msg = NULL,
            lvl = NULL,
            has_error = FALSE)

  valid_disaggs <- d$info$schema %>%
    dplyr::filter(sheet_name %in% sheets
                  & col_type == "target") %>%
    dplyr::select(sheet_name, indicator_code,
                  valid_ages, valid_sexes, valid_kps) %>%
    tidyr::unnest(valid_ages, names_sep = ".") %>%
    tidyr::unnest(valid_sexes, names_sep = ".") %>%
    tidyr::unnest(valid_kps, names_sep = ".") %>%
    dplyr::select(sheet_name, indicator_code, Age = valid_ages.name,
                  Sex = valid_sexes.name, KeyPop = valid_kps.name)

  defunct_disaggs <- unPackDataPackSheet(d,
                                         sheets,
                                         clean_orgs = TRUE,
                                         clean_disaggs = FALSE,
                                         clean_values = TRUE) %>%
    dplyr::anti_join(
      valid_disaggs,
      by = c("sheet_name", "indicator_code", "Age", "Sex", "KeyPop")) %>%
    dplyr::select(sheet_name, indicator_code, Age, Sex, KeyPop) %>%
    dplyr::distinct()

  if (NROW(defunct_disaggs) > 0) {

    ch$lvl <- "ERROR"

    ch$msg <- unique(defunct_disaggs$sheet_name) %>%
      purrr::set_names() %>%
      purrr::map(
        function(x) {
          paste0(
            ch$lvl, "! In tab ", x,
            ": INVALID DISAGGS. Please review all tabs flagged by this test to ensure",
            " no Age, Sex, or Key Population disaggregates have been inadvertently or",
            " incorrectly altered. If you believe this has been flagged in error, ",
            " please first refer to MER Guidance to confirm valid disaggregates for",
            " the data element flagged. (Check MER Guidance for correct alternatives.",
            " Also note that single-digit ages should be left-padded with zeros,",
            " e.g., 01-04 instead of 1-4.) -> \n\t",
            paste(
              utils::capture.output(
                print(
                  as.data.frame(
                    defunct_disaggs[defunct_disaggs$sheet_name == x, ]),
                  row.names = FALSE)),
              collapse = "\n\t"),
            "\n")
        })

    ch$result <- defunct_disaggs
    attr(ch$result, "test_name") <- "Defunct disaggs"
    ch$has_error <- TRUE

    if (!quiet) {
      messages <- MessageQueue()
      for (i in seq_along(ch$msg)) {
        messages <- appendMessage(messages, ch$msg[[i]], ch$lvl)
      }
      printMessages(messages)
    }
  }

  return(ch)
}


#' @export
#' @rdname unPackDataChecks
#'
checkToolEmptySheets <- function(d, sheets, quiet = TRUE) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  # Check if all key header columns missing
  header_cols <- purrr::map(sheets, function(x) {
    d$info$schema %>%
      dplyr::filter(sheet_name %in% x,
                    col_type == "row_header", !indicator_code %in% c("SNU1", "ID")) %>%
      dplyr::pull(indicator_code) %>%
      #c(., "mechCode_supportType") %>% # DP-472
      unique()
  })

  has_all_header_columns <-
    purrr::map2(d$sheets[sheets], header_cols,
                function(x, y) {
                  Reduce("+", y %in% names(x)) == length(y)
                }) %>%
    unlist()

  if (!all(has_all_header_columns)) {

    lvl <- "ERROR"

    msg <-
      paste0(
        lvl, "! MISSING KEY COLUMNS: The following sheets are missing critical ",
        "columns, usually PSNU, Age, Sex, and/or KeyPop. This prevents us from ",
        "checking and reading any data from these sheets. -> \n  * ",
        paste0(sheets[!has_all_header_columns], collapse = "\n  * "),
        "\n")

    d$tests$missing_index_columns <- data.frame(sheet_name = sheets[!has_all_header_columns])
    attr(d$tests$missing_index_columns, "test_name") <- "Missing index columns"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
  }

  # Check if no rows of data
  has_rows_data <-
    purrr::map(d$sheets[sheets],
               function(x) {
                 NROW(x) > 0
               }) %>%
    unlist()

  if (!all(has_rows_data)) {

    lvl <- "INFO"

    msg <-
      paste0(
        lvl, "! SHEETS WITH NO DATA: The following sheets appear to have no ",
        "rows of data. If this is intentional, no need to worry. -> \n  * ",
        paste0(sheets[!has_rows_data], collapse = "\n  * "),
        "\n")

    d$tests$no_rows_data <- data.frame(sheet_name = sheets[!has_rows_data])
    attr(d$tests$no_rows_data, "test_name") <- "No rows of data"
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

  data_checks <-  purrr::map(funs, purrr::exec, sheets, d)

  d$tests <-
    append(d$tests,
           purrr::map(data_checks, "result")) %>%
    purrr::discard(is.null)

  msg <- purrr::map(data_checks, ~ Reduce(f = c,
                                              x = purrr::pluck(.x, "msg"))) %>%
    Reduce(f = c, x = .)
  lvl <- purrr::map(data_checks,
                    function(x) {
                      rep(purrr::pluck(x, "lvl"),
                          length(purrr::pluck(x, "msg")))
                    }) %>%
    Reduce(f = c, x = .)

  for (i in seq_along(msg)) {
    d$info$messages <- appendMessage(d$info$messages, msg[i], lvl[i])
  }

  d$info$has_error <-
    purrr::map_lgl(data_checks, "has_error") %>%
    c(., d$info$has_error) %>%
    any()

  return(d)

}
