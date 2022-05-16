#' Perform various data quality checks on in-process data from Data Packs.
#'
#' @description
#' A series of functions to check and validate quality & integrity
#' of data encountered in various `unPack...` functions across `datapackr`. Note
#' that these functions do not attempt to correct issues identified, but only to
#' identify them.
#' 
#' `checkDupeRows` checks for any rows with duplicates across PSNU and other key
#' disaggregates.
#' 
#' `checkColumnStructure` checks structural integrity of columns on critical sheets for
#'    a submitted Data Pack.
#' 
#' `checkToolStructure` checks structural integrity of sheets for submitted tool.
#' 
#' `checkToolComments` searches Data Pack for any comments that cause
#' corruption when executing openxlsx::saveWorkbook.
#' 
#' `checkConnections` detects the presence of any external links in a Tool. 
#' 
#' `checkNumeric` alerts to non-numeric values instead of valid data.
#'
#' @name unPackDataChecks
#' @md
#' @importFrom magrittr %>% %<>%
#'
#' @param d DataPack object loaded via `loadDataPack`.
#' @param sheet String. Name of DataPack sheet to check data from. Default is
#'   first sheet.
#' @param quiet Logical. Should warning messages be printed? Default is TRUE.
#' @param compile Logical. Should results be compiled back into the d object, or
#'   returned as a test results object containing `test_results`, `messages`,
#'   and `error` flag? Default is TRUE
#'
#' @return If `compile = TRUE`, a DataPack object, with updated tests and warnings.
#'   If `compile = FALSE`, a test results object containing `test_results`,
#'   `messages`, and `error` flag.
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
  
  # Get data ----
  data <- d$sheets[[as.character(sheet)]]
  
  # Get header_cols ####
  header_cols <- d$info$schema %>%
    dplyr::filter(
      sheet_name == sheet,
      col_type == "row_header") %>%
    dplyr::pull(indicator_code) %>%
    c(., "mechCode_supportType")
  
  header_cols <- header_cols[header_cols %in% names(data)]
  
  # Drop rows/cols with all NAs or 0s ----
  # We don't care if these are duplicates
  names(data) <- data %>%
    names() %>%
    make.names() %>%
    make.unique()
  
  data %<>%
    #dplyr::select(header_cols, where(~ any(!is.na(.x)))) %>%
    { suppressWarnings(
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
  
  # Duplicates ----
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
  
  # Get data ----
  data <- d$sheets[[as.character(sheet)]]
  
  # Cross-check cols ----
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
  
  if (is.null(d$tool$wb)) {
    wb <- openxlsx::loadWorkbook(file = d$keychain$submission_path)
  } else {
    wb <- d$tool$wb
  }
  
  # d$info$has_comments_issue <-
  #   any(unlist(lapply(wb$comments, function(x) is.null(x["style"]))))
  
  d$info$has_comments_issue <- any(sapply(wb$threadComments, length) != 0)
  
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
checkConnections <- function(d, quiet = TRUE) {

  d$info$workbook_contents <- unzip(d$keychain$submission_path, list = TRUE) %>%
    dplyr::pull(`Name`)

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
checkNumeric <- function(d, sheet, quiet = TRUE) {
  
  if (!quiet) {
    messages <- MessageQueue()
  }
  
  # Get data ----
  data <- d$sheets[[as.character(sheet)]]
  
  # 
  
  keep_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  !is.na(indicator_code),
                  !indicator_code %in% c("sheet_num", "ID", "SNU1"),
                  col_type %in% c("row_header", "target", "result"))
  
  if (d$info$tool == "OPU Data Pack") {
    data %<>%
      tidyr::gather(key = "mechCode_supportType",
                    value = "value",
                    -tidyselect::all_of(header_cols$indicator_code)) %>%
      dplyr::select(dplyr::all_of(header_cols$indicator_code),
                    mechCode_supportType, value) %>%
      tidyr::drop_na(value)
  }
  
  if (d$info$tool == "Data Pack" & sheet == "PSNUxIM" & d$info$cop_year %in% c(2021, 2022)) {
    data %<>%
      tidyr::gather(key = "mechCode_supportType",
                    value = "value",
                    -tidyselect::all_of(c(header_cols$indicator_code))) %>%
      dplyr::select(dplyr::all_of(header_cols$indicator_code), -indicator_code,
                    indicator_code = mechCode_supportType, value) %>%
      tidyr::drop_na(value)
  }
  
  header_cols <- keep_cols %>%
    dplyr::filter(col_type == "row_header") %>%
    dplyr::pull(indicator_code)
  
  non_numeric <- data %>%
    dplyr::select(tidyselect::any_of(keep_cols$indicator_code)) %>%
    tidyr::pivot_longer(cols = !tidyselect::all_of(header_cols),
                        names_to = "indicator_code",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(value_numeric = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(is.na(value_numeric)) %>%
    dplyr::select(indicator_code, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(indicator_code) %>%
    dplyr::arrange(value) %>%
    dplyr::summarise(values = paste(value, collapse = ", ")) %>%
    dplyr::mutate(row_id = paste(indicator_code, values, sep = ":  ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::select(row_id) %>%
    dplyr::mutate(sheet = sheet)
  
  if (NROW(non_numeric) > 0) {
    lvl <- "WARNING"
    
    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ": NON-NUMERIC VALUES found! Please ensure all values entered against",
        " FY22 Target columns include numeric values only - no letters or punctuation.",
        " It may be helpful to use an Excel filter to check unique values in a column for",
        " any non-numeric entries. ->  \n\t* ",
        paste(non_numeric$row_id, collapse = "\n\t* "),
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
  
  # Get data ----
  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {
    
    data <- d$sheets[["PSNUxIM"]]
  } else {
    data <- d$sheets[[as.character(sheet)]]
  }
  
  # mung ----
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  
  missing_metadata <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row = dplyr::row_number() + header_row,
                  sheet = sheet) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("^PSNU$|^ID$|^indicator_code$")),
                     dplyr::any_vars(is.na(.)))
  
  # test ----
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
  
  # get data ----
  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {
    
    data <- d$sheets[["PSNUxIM"]]
  } else {
    data <- d$sheets[[as.character(sheet)]]
  }
  
  # mung ----
  target_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & (col_type == "target" | (col_type == "result" & dataset == "subnat"))
                  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(data)) %>%
    dplyr::pull(indicator_code)
  
  if (NROW(target_cols) == 0) {
    data <- NULL
    return(d)
  }
  
  # Add cols to allow compiling with other sheets ####
  data <- data %>%
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
  
  # If PSNU has been deleted, drop the row ####
  data <- data %>%
    dplyr::filter(!is.na(PSNU))
  
  # Gather all indicators as single column for easier processing ####
  data <- data %>%
    tidyr::gather(key = "indicator_code",
                  value = "value",
                  -PSNU, -psnuid, -Age, -Sex, -KeyPop, -sheet_name) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value)
  
  # Drop NAs ####
  data <- data %>%
    tidyr::drop_na(value)
  
  # Now that non-numeric cases noted, convert all to numeric & drop non-numeric ####
  data <- data %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value) %>%
    # Filter out zeros ####
  dplyr::filter(value != 0)
  
  # test ----
  negative_values <- data %>%
    dplyr::filter(value < 0)
  
  if (NROW(negative_values) > 0) {
    lvl <- "ERROR"
    
    msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
        " against Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
        paste(unique(d$tests$negative_values$indicator_code), collapse = "\n\t* "),
        "\n")
    
    d$tests$negative_values <- dplyr::bind_rows(d$test$negative_values, negative_values)
    attr(d$tests$negative_values, "test_name") <- "Negative values"
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
checkDecimalValues <- function(d, sheet, quiet = T) {
  if (!quiet) {
    messages <- MessageQueue()
  }
  
  # get data ----
  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {
    
    data <- d$sheets[["PSNUxIM"]]
  } else {
    data <- d$sheets[[as.character(sheet)]]
  }
  
  
  # mung ----
  # List Target Columns
  target_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  &
                    (
                      col_type == "target" | (col_type == "result" & dataset == "subnat")
                    )
                  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(data)) %>%
    dplyr::pull(indicator_code)
  
  if (NROW(target_cols) == 0) {
    data <- NULL
    return(d)
  }
  
  # Add cols to allow compiling with other sheets
  data <- data %>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
    # Select only target-related columns
    dplyr::select(PSNU, Age, Sex, KeyPop,
                  dplyr::one_of(target_cols)) %>%
    # Drop rows where entire row is NA
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
    # Extract PSNU uid
    dplyr::mutate(
      psnuid = stringr::str_extract(
        PSNU,
        "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"
      ),
      # Tag sheet name
      sheet_name = sheet
    ) %>%
    dplyr::select(PSNU,
                  psnuid,
                  sheet_name,
                  Age,
                  Sex,
                  KeyPop,
                  dplyr::everything())
  
  # If PSNU has been deleted, drop the row
  data <- data %>%
    dplyr::filter(!is.na(PSNU))
  
  # Gather all indicators as single column for easier processing
  data <- data %>%
    tidyr::gather(key = "indicator_code",
                  value = "value", -PSNU,
                  -psnuid,
                  -Age,
                  -Sex,
                  -KeyPop,
                  -sheet_name) %>%
    dplyr::select(PSNU,
                  psnuid,
                  sheet_name,
                  indicator_code,
                  Age,
                  Sex,
                  KeyPop,
                  value)
  
  # Drop NAs
  data <- data %>%
    tidyr::drop_na(value)
  
  # Now that non-numeric cases noted, convert all to numeric & drop non-numeric
  data <- data %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value) %>%
    # Filter out zeros
    dplyr::filter(value != 0)
  
  # TEST: actual test begins ----
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
    dplyr::filter(value %% 1 != 0
                  & !indicator_code %in% decimals_allowed) %>%
    dplyr::rename(sheet = sheet_name)
  
  # test ----
  if (NROW(decimal_cols) > 0) {
    lvl <- "WARNING"
    
    msg <-
      paste0(
        lvl,
        "! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns! Ensure all values entered",
        " against Targets are whole, positive, numeric values. (The only exception",
        " to this rule may be HIV_PREV.) These will be rounded. -> \n\t* ",
        paste(unique(decimal_cols$indicator_code), collapse = "\n\t* "),
        "\n"
      )
    
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
checkSheetData <- function(d,
                           sheets = NULL,
                           quiet = TRUE,
                           ...) {
  
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
  
  for (sheet in sheets) {
    # Col Structure ----
    d <- checkColumnStructure(d, sheet)
    
    # Duplicate Rows ----
    d <- checkDupeRows(d, sheet)
    
    # Numeric Values ----
    d <- checkNumeric(d, sheet)  
    
    # Metadata ----
    d <- checkMissingMetadata(d, sheet)
    
    # Negative values ----
    d <- checkNegativeValues(d, sheet)
    
    # Decimal values
    d <- checkDecimalValues(d, sheet)
    
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
    #     warning_msg <- paste0(
    #       "WARNING! In tab ",
    #       sheet,
    #       ": MISSING DREAMS SNUs found! ->  \n\t* ",
    #       paste(missing_DSNUs$PSNU.y, collapse = "\n\t* "),
    #       "\n")
    #     
    #     d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    #     d$info$missing_DSNUs <- TRUE
    #   }
    #   
    #   if (any(is.na(DSNU_comparison$DATIM))) {
    #     invalid_DSNUs <- DSNU_comparison %>%
    #       dplyr::filter(is.na(DATIM))
    #     
    #     warning_msg <- paste0(
    #       "WARNING! In tab ",
    #       sheet,
    #       ": INVALID DREAMS SNUs found! ->  \n\t* ",
    #       paste(invalid_DSNUs$PSNU.x, collapse = "\n\t* "),
    #       "\n")
    #     
    #     d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    #   }
    #   
    # }
    
    # Formulas ----
    # d <- checkFormulas(d, sheet)
    
    # TEST that all Prioritizations completed ####
    # if (sheet == "Prioritization") {
    #   
    #   # Remove _Military district from Prioritization extract as this can't be assigned a prioritization ####
    #   d$data$extract %<>%
    #     dplyr::filter(!stringr::str_detect(PSNU, "^_Military"),
    #                   # Excuse valid NA Prioritizations
    #                   value != "NA"
    #     )
    #   
    #   blank_prioritizations <- d$data$extract %>%
    #     dplyr::filter(is.na(value)) %>%
    #     dplyr::select(PSNU)
    #   
    #   if (NROW(blank_prioritizations) > 0) {
    #     
    #     d$tests$blank_prioritizations <- blank_prioritizations
    #     attr(d$tests$blank_prioritizations, "test_name") <- "Blank prioritization levels"
    #     
    #     warning_msg <-
    #       paste0(
    #         "ERROR! In tab ",
    #         sheet,
    #         ": MISSING PRIORITIZATIONS. Ensure a prioritization value is entered in each",
    #         " row of the column labeled 'SNU Prioritization' on the Prioritization tab.",
    #         " Refer to guidance on that tab and in the Data Pack User Guide to see",
    #         " appropriate entry options. You must enter a prioritization value for",
    #         " the following PSNUs -> \n\t* ",
    #         paste(blank_prioritizations$PSNU, collapse = "\n\t* "),
    #         "\n")
    #     
    #     d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    #     d$info$has_error <- TRUE
    #     
    #   }
    #   
    #   # Test for valid priortization values
    #   invalid_prioritizations <- d$data$extract %>%
    #     dplyr::filter(!(value %in% c("1", "2", "4", "5", "6", "7", "8")))
    #   
    #   
    #   if (NROW(invalid_prioritizations) > 0) {
    #     d$tests$invalid_prioritizations <- invalid_prioritizations
    #     attr(d$tests$invalid_prioritizations, "test_name") <- "Invalid prioritizations"
    #     
    #     invalid_prio_strings <- invalid_prioritizations %>%
    #       tidyr::unite(row_id, c(PSNU, value), sep = ":  ") %>%
    #       dplyr::arrange(row_id) %>%
    #       dplyr::pull(row_id)
    #     
    #     warning_msg <-
    #       paste0(
    #         "ERROR! In tab ",
    #         sheet,
    #         ": INVALID PRIORITIZATIONS. The following Prioritizations are not valid for",
    #         " the listed PSNUs. Review the guidance on the Prioritization tab and in the",
    #         " Data Pack User Guide to understand valid prioritization options. Refer to those",
    #         " PSNUs flagged by this check and correct their validation values in the 'SNU Prioritization'",
    #         " column on the Prioritization tab. -> \n\t* ",
    #         paste(invalid_prio_strings, collapse = "\n\t* "),
    #         "\n")
    #     
    #     d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    #     d$info$has_error <- TRUE
    #   }
    #   
    # }
    
    # TEST: No invalid org units ####
    # d <- checkInvalidOrgUnits(d, sheet)
    
    # TEST for Negative values ####
    # negative_values <- d$data$extract %>%
    #   dplyr::filter(value < 0)
    # 
    # d$tests$negative_values <- dplyr::bind_rows(d$test$negative_values, negative_values)
    # attr(d$tests$negative_values, "test_name") <- "Negative values"
    # 
    # if (NROW(negative_values) > 0) {
    #   
    #   warning_msg <-
    #     paste0(
    #       "ERROR! In tab ",
    #       sheet,
    #       ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
    #       " against Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
    #       paste(unique(d$tests$negative_values$indicator_code), collapse = "\n\t* "),
    #       "\n")
    #   
    #   d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    #   d$info$has_error <- TRUE
    # }
    
    # TEST for Decimal values ####
    # decimals_allowed <- d$info$schema %>%
    #   dplyr::filter(sheet_name == sheet
    #                 & col_type == "target"
    #                 # Filter by what's in submission to avoid unknown column warning messages
    #                 & indicator_code %in% unique(d$data$extract$indicator_code)
    #                 & value_type == "percentage") %>%
    #   dplyr::pull(indicator_code)
    # 
    # decimal_cols <- d$data$extract %>%
    #   dplyr::filter(value %% 1 != 0
    #                 & !indicator_code %in% decimals_allowed) %>%
    #   dplyr::rename(sheet = sheet_name)
    # 
    # d$tests$decimal_values <- dplyr::bind_rows(d$tests$decimal_cols, decimal_cols)
    # attr(d$tests$decimal_values, "test_name") <- "Decimal values"
    # 
    # if (NROW(decimal_cols) > 0) {
    #   
    #   warning_msg <-
    #     paste0(
    #       "WARNING! In tab ",
    #       sheet,
    #       ": DECIMAL VALUES found in the following columns! Ensure all values entered",
    #       " against Targets are whole, positive, numeric values. (The only exception",
    #       " to this rule may be HIV_PREV.) These will be rounded. -> \n\t* ",
    #       paste(unique(decimal_cols$indicator_code), collapse = "\n\t* "),
    #       "\n")
    #   
    #   d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    # }
    
    # TEST for defunct disaggs ####
    # d <- defunctDisaggs(d, sheet)
    
  }
  
  return(d)
  
}
