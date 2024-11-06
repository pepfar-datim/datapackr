#' Check that a Data Pack tool has a PSNUxIM tab
#'
#' @inheritParams datapackr_params
#' @description Internal function to determine if a PSNUxIM tab exists
#'
#' @return d object list with additional flags for PSNUxIM state.
checkHasPSNUxIM <- function(d) {

  stopifnot(is.data.frame(d$data$SNUxIM))

  if (NROW(d$data$SNUxIM) == 1 && is.na(d$data$SNUxIM[[1, 1]])) {
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
    } else if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {
      warning_msg <- paste0(
        "WARNING! Your OPU Data Pack's PSNUxIM tab appears to be empty. Please",
        " investigate and resubmit."
      )
    }

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)

  } else {
    d$info$has_psnuxim <- TRUE
  }

  return(d)

}

#' Title extractSNUxIMCombos for a given datapack (d)
#' and a given PSNUxIM object (p) extract all of the missing
#' combos which exist in in the DataPack but do not exist
#' in the PSNUxIM tab. Note that a combo in this context actually refers
#' to a combination of PSNU, indicator_code, Age, Sex and KeyPop
#' so does not contain any IM identifier.
#'
#' @inheritParams datapackr_params
#' @param p An optional PSNUxIM object.
#' @return Modified d object with documented and missing SNUxIM combos
#'
extractSNUxIMCombos <- function(d, p = NULL) {

  if (is.null(d$data$SNUxIM) && is.null(p$data$SNUxIM)) {
    stop("PSNUxIM cannot be null")
  }

  if (!is.null(p$data$PSNUxIM_combos))  {
    d$data$PSNUxIM_combos <- p$data$PSNUxIM_combos
  } else {
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
  }

  if (d$info$tool == "Data Pack") {

    if (is.null(d$data$MER)) {
      stop("MER data cannot be null.")
    }

    d$data$missingCombos <- d$data$MER %>%
      dplyr::filter(!indicator_code %in% c("AGYW_PREV.D.T", "AGYW_PREV.N.T")) %>%
      # Special handling for differences between main tab and PSNUxIM tab age bands
      # The data should not be aggregated at this point. This will happen
      # when the data is repacked by packForDATIM_UndistributedMER
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

      d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)

    }
  }

  d
}


#' Title extractDuplicateRows
#'
#' Identifies Duplicate rows in the PSNUxIM tab.
#'
#' @inheritParams datapackr_params
#' @param sheet Name of the sheet (PSNUxIM)
#' @return Modified d object with a list of duplicated rows
#'
extractDuplicateRows <- function(d, sheet = "PSNUxIM") {

  if (is.null(d$data$SNUxIM)) {
    stop("PSNUxIM cannot be null")
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

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
    d$info$has_error <- TRUE

  }

  d

}

#' getColumnsToKeep(d)
#'
#' Return a vector of columsn names to keep for this tool.
#'
#' @param d Datapackr d object.
#' @param sheet Name of the sheet (PSNUxIM)
#'
#' @return A list of columns to keep.

getColumnsToKeep <- function(d, sheet = "PSNUxIM") {

  #If we have a hybrid tool, then use the PSNUxIM schema
  #Otherwise, for a standalone PSNUxIM tool, the schema
  #will be in the normal location d$info$schema

  if (isTRUE(d$info$hybrid_tool)) {
    schema <- d$info$psnuxim_schema
  } else {
    schema <- d$info$schema %>%
      dplyr::filter(sheet_name == sheet)
  }

  schema %>%
    dplyr::filter(!is.na(indicator_code),
                  !indicator_code %in% c("sheet_num", "ID", "SNU1"),
                  col_type %in% c("row_header", "target"))
}

#' getHeaderColumns returns a vector of header columns
#' provided an input vector of columns to keep.
#'
#' @param cols_to_keep cols_to_keep object
#' @param sheet Name of the sheet (PSNUxIM)
#'
#' @return A vector of header columns.
#'

getHeaderColumns <- function(cols_to_keep, sheet = "PSNUxIM") {

  if (is.null(cols_to_keep)) {
    cols_to_keep <- getColumnsToKeep(d, sheet)
  }

  cols_to_keep %>%
    dplyr::filter(col_type == "row_header")
}

#' checkNonEqualTargets extracts all rows from the PSNUxIM tab
#' where the value of the DataPackTarget (currently column G)
#' does not match the corresponding value in the main DataPackTabs.
#'
#' @param d Datpackr d object
#' @param original_targets original_targets object
#'
#' @return Modified d object
checkNonEqualTargets <- function(d, original_targets) {


  if (d$info$tool == "Data Pack") {
    #Check to ensure that the value in column G (DataPack Target) actually
    #matches the data in the main tabs

    main_tab_data <- original_targets %>%
      dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, MainTabsTarget = value) %>%
      dplyr::filter(!indicator_code %in% c("AGYW_PREV.D.T", "AGYW_PREV.N.T")) %>%
      # Special handling for differences between main tab and PSNUxIM tab age bands
      # and the original tabs
      dplyr::mutate(Age = dplyr::case_when(
        stringr::str_detect(Age, "(50-54|55-59|60-64|65+)") &
          !stringr::str_detect(indicator_code, "TX_CURR.T") ~ "50+",
        TRUE ~ Age
      )) %>%
      dplyr::group_by(dplyr::across(c(-MainTabsTarget))) %>%
      dplyr::summarise(MainTabsTarget = sum(MainTabsTarget, na.rm = TRUE), .groups = "drop")

    #Grab this from the raw sheet data prior to any processing/reshaping
    snu_targets <-  d$sheets$PSNUxIM

    d$tests$non_equal_targets  <- snu_targets %>%
      dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, DataPackTarget) %>%
      dplyr::mutate(DataPackTarget = as.numeric(DataPackTarget)) %>%
      dplyr::full_join(main_tab_data, by = c("PSNU", "indicator_code", "Age", "Sex", "KeyPop")) %>%
      dplyr::mutate(are_equal = dplyr::near(DataPackTarget, MainTabsTarget, tol = 0.1)) %>%
      #If the main tab value is missing and the DataPackTarget is zero, ignore
      dplyr::mutate(are_equal = dplyr::case_when(is.na(MainTabsTarget) & DataPackTarget == 0 ~ TRUE,
                                                 is.na(MainTabsTarget) & DataPackTarget != 0 ~ FALSE,
                                                 TRUE ~ are_equal)) %>%
      dplyr::filter(!are_equal | is.na(are_equal)) %>%
      #Filter non-allocated data to prevent false positives with this test
      #Other tests should catch whether there is data in the main tabs
      #but which has not been allocated
      dplyr::filter(!is.na(DataPackTarget)) %>%
      dplyr::rename("PSNUxIM Target" = DataPackTarget)

    attr(d$tests$non_equal_targets, "test_name") <- "Non-equal targets"

    if (NROW(d$tests$non_equal_targets) > 0) {
      warning_msg <-
        paste(
          "ERROR! In tab PSNUxIM:", NROW(d$tests$non_equal_targets),
          "instances of values in column G (DataPackTargets) which do not",
          "equal the targets set in the main tabs. Please check to ensure",
          "that the formulas in column G are correct. Please",
          "download a copy of the validation report from the self-service app",
          "and consult the tab non_equal_targets for details.\n")

      d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
      d$info$has_error <- TRUE
    }

  }

  d

}

#' Title
#'
#' @param d Datapackr d object
#' @param cols_to_keep cols_to_keep object
#' @param header_cols headers_cols object
#' @param sheet Name of the sheet (PSNUxIM)
#'
#' @return original_targets object as a data frame.
#'
extractOriginalTargets <- function(d, cols_to_keep, header_cols, sheet = "PSNUxIM") {

  if (d$info$tool == "Data Pack") {
    d$data$MER
  } else {
    d$data$SNUxIM %>%
      dplyr::select(header_cols$indicator_code, "DataPackTarget") %>%
      { suppressWarnings(dplyr::mutate_at(., dplyr::vars(-dplyr::all_of(header_cols$indicator_code)), # nolint
                                          as.numeric))
      } %>%
      dplyr::group_by(dplyr::across(header_cols$indicator_code)) %>%
      dplyr::summarise(DataPackTarget = sum(DataPackTarget), .groups = "drop") %>%
      dplyr::mutate(
        psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"),
        sheet_name = sheet
      ) %>%
      dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop,
                    value = DataPackTarget)
  }

}

#' Title testMissingRightSideFormulas
#'
#' @param d Datapackr d object
#' @param cols_to_keep cols_to_keep object
#' @param header_cols headers_cols object
#' @param header_row Numeric index of the header row in the PSNUxIM tab.
#' @param blank_cols_idx Index of the first column which is blank in the sheet.
#' @param parsed_cells Primarily used for unit testing. When null,
#' values will be read from the PSNUxIM tab.
#'
#' @return Modified d object with data frame of columns with missing formulas.
#'
testMissingRightSideFormulas <- function(d, cols_to_keep, header_cols,
                                         header_row, blank_cols_idx, parsed_cells = NULL) {

  if (is.null(parsed_cells)) {

    #For hybrid tools, choose the PSNUxIM file.
    psnuxim_path <-
      ifelse(
        !is.null(d$keychain$psnuxim_file_path),
        d$keychain$psnuxim_file_path,
        d$keychain$submission_path
      )

    parsed_cells <-  tidyxl::xlsx_cells(path = psnuxim_path,
                                        sheets = "PSNUxIM",
                                        include_blank_cells = TRUE)
  }

  # TEST: Missing right-side formulas; Warn; Continue ####
  d$tests$psnuxim_missing_rs_fxs <- parsed_cells %>%
    dplyr::select(col, row, formula, character) %>%
    dplyr::filter(row >= header_row,
                  col %in% cols_to_keep$col) %>%
    dplyr::filter(!col %in% header_cols$col) %>%
    dplyr::mutate(formula = dplyr::if_else(is.na(formula),
                                           character,
                                           formula)) %>%
    dplyr::select(-character) %>%
    dplyr::filter(is.na(formula)) %>%
    dplyr::mutate(col_letter = openxlsx::int2col(col)) %>%
    #Ignore missing right side formulas in columns which have no header information
    dplyr::filter(!(col %in% blank_cols_idx))

  attr(d$tests$psnuxim_missing_rs_fxs, "test_name") <- "Missing PSNUxIM R.S. Formulas"

  if (NROW(d$tests$psnuxim_missing_rs_fxs) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab PSNUxIM: MISSING FORMULAS ON RIGHT SIDE.",
        " Make sure all formulas in the far right section of your PSNUxIM tab",
        " (section titled 'Target Values') are completely copied to the bottom",
        " of your data. The following columns are implicated. -> \n\t",
        paste(sort(unique(d$tests$psnuxim_missing_rs_fxs$col_letter)), collapse = ", "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)
  }

  d
}

#' Title dropDuplicatedPSNUxIMColumns
#' @description Identify and drop any duplicated user specified mechanism
#' columns in the PSNUxIM tab.
#'
#' @param d Datapackr d object
#'
#' @return Modified d object
dropDuplicatedPSNUxIMColumns <- function(d) {

  #Test for duplicate columns
  dup_cols <- names(d$data$SNUxIM)[duplicated(names(d$data$SNUxIM))]

  if (length(dup_cols) > 0) {
    warning_msg <-
      paste0(
        "ERROR! In tab PSNUxIM: DUPLICATE MECHANISM COLUMNS",
        " Ensure that all mechanisms columns are unique in both the percentage",
        " allocation section as well as the value section of the PSNUxIM tab.",
        " The following columns are implicated. -> \n\t",
        paste(dup_cols, sep = "", collapse = ","),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)

    #Drop the duplicated columns and continue
    d$data$SNUxIM <- d$data$SNUxIM[, !duplicated(names(d$data$SNUxIM))]
    warning("Dropping duplicated columns in the PSNUxIM tab.")

  }

  d

}

#' Title dropInvalidMechColumns
#' @description Removes any invalid, user-specified mechanism
#' columns in the PSNUxIM tab.
#'
#' @param d Datpackr d object
#' @param cols_to_keep Object of columns to keep
#' @param sheet Name of the sheet
#'
#' @return d
dropInvalidMechColumns <- function(d, cols_to_keep, sheet = "PSNUxIM") {

  # nolint
  # nolint start
  # TEST: Improper Col Names; Error; Drop ####
  invalid_mech_headers <- names(d$data$SNUxIM) %>%
    tibble::tibble(col_name = .) %>%
    dplyr::filter(!col_name %in% cols_to_keep$indicator_code,
                  !(stringr::str_detect(col_name, "(\\d){4,6}")
                    & stringr::str_detect(col_name, "DSD|TA"))) %>%
    dplyr::filter(col_name != "Not PEPFAR") #Specifically allow for "Not PEPFAR"
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

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
    d$info$has_error <- TRUE
  }

  d$data$SNUxIM %<>%
    dplyr::select(-dplyr::all_of(d$tests$invalid_mech_headers$invalid_mech_headers))

  d

}



#' Title checkPSNUxIMDisaggs
#' @description Compares Age/Sex/KeyPop disaggs found in the PSNUxIM tab
#' with those found in the DE/COC map for a given year.
#' @param d Datapackr d object
#'
#' @return d Datapackr d object
#'

checkPSNUxIMDisaggs <- function(d) {

  header_row <- headerRow(d$info$tool, d$info$cop_year)
  #We use the DATIM DE/COC map here even if this is a datapack
  #Since we assume that the disaggs in the PSNUxIM tab match
  #DATIM.
  de_coc_map <-
    getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year, datasource = "PSNUxIM") %>%
    dplyr::select(indicator_code,
                  "Age" = valid_ages.name,
                  Sex = valid_sexes.name,
                  "KeyPop" = valid_kps.name) %>%
    dplyr::mutate(exists = TRUE) %>%
    dplyr::distinct() #Ignore differences with DSD and TA at this point

  data <- d$data$SNUxIM %>%
    dplyr::select(PSNU, indicator_code, "Age", "Sex", "KeyPop") %>%
    dplyr::mutate(row_number = dplyr::row_number() + header_row)


  defunct_disaggs <-
    dplyr::left_join(data, de_coc_map, by = c("indicator_code", "Age", "Sex", "KeyPop"))

  if (any(is.na(defunct_disaggs$exists))) {

    d$tests$invalid_psnuxim_disaggs <- defunct_disaggs %>%
      dplyr::filter(is.na(exists)) %>%
      dplyr::select(-exists)

    affected_rows <- d$tests$invalid_psnuxim_disaggs$row_number

    attr(d$tests$invalid_psnuxim_disaggs, "test_name") <- "Invalid PSNUxIM Disaggs"

    lvl <- "ERROR"

    msg <-
      paste0(lvl, "! In tab PSNUxIM ", length(affected_rows),
             " invalid disaggregate combinations found. Please review all rows of data flagged by this test to ensure",
             " no Age, Sex, or Key Population disaggregates have been inadvertently or",
             " incorrectly altered. If you believe this has been flagged in error, ",
             " please first refer to MER Guidance to confirm valid disaggregates for",
             " the data element flagged. (Check MER Guidance for correct alternatives.",
             " Also note that single-digit ages should be left-padded with zeros,",
             " e.g., 01-04 instead of 1-4.)",
             " The following rows are implicated: ", formatSetStrings(affected_rows), "\n\t")

    d$info$messages <- appendMessage(d$info$messages, msg, lvl, d$info$tool)
    d$info$has_error <- TRUE
  }

  d


}

combineDuplicatePSNUxIMColumns <- function(d, cols_to_keep) {

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

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)
  }

  # --> This also removes non-essential text from IM name to leave only 12345_DSD format.

  names(d$data$SNUxIM) <- col_names$col_name_new


  d

}

#' Title checkNonNumericPSNUxIMValues
#' @description
#' Checks for non-numeric data values in the PSNUxIM tab.
#'
#' @param d Datapackr d object
#' @param header_cols header_cols object
#'
#' @return Datapackr d object
#'
checkNonNumericPSNUxIMValues <- function(d, header_cols) {

  df <- d$data$SNUxIM %>%
    dplyr::select(-tidyselect::any_of(header_cols$indicator_code))

  #Find anything which is not numeric.
  #Note, decimals and negatives are allowed.
  df_list <- lapply(as.list(df), function(x) which(!(grepl("^0|(-)?[1-9]\\d*(\\.\\d+)?$", x) | is.na(x))))
  #Remove empty lists
  df_list <- purrr::compact(df_list)

  if (length(df_list) > 0) {
    d$tests$non_numeric_psnuxim_values <- data.frame(columns = names(df_list),
                                                     rows = do.call(rbind, lapply(df_list, formatSetStrings)))

    attr(d$tests$non_numeric_psnuxim_values, "test_name") <- "Non-numeric PSNUxIM values"
    warning_msg <-
      paste0(
        "WARNING! In tab PSNUxIM: Non-numeric values! found in columns",
        paste0(names(df_list), sep = "", collapse = ","),
        ". Please consult the validation report for the specific rows where these values were found.\n\t* ",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)

  }

  d

}

#' Title testMissingDedupeRollupColumns
#'
#' @param d Datapackr d object
#' @param cols_to_keep cols_to_keep object
#' @param sheet Name of the sheet
#'
#' @return Datapackr d object

testMissingDedupeRollupColumns <- function(d, cols_to_keep, sheet = "PSNUxIM") {

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

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
    d$info$has_error <- TRUE
  }

  d$data$SNUxIM %<>%
    datapackr::addcols(
      missing_cols_fatal,
      type = "numeric")

  d
}

#' Title recalculateDedupeValues
#' Recalculates dedupe values based on the provided duplicated and deduplicated values.
#' @param d Datapackr d object
#'
#' @return Datapackr d object
recalculateDedupeValues <- function(d) {
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
        pmax(`Deduplicated DSD Rollup`, `Deduplicated TA Rollup`, na.rm = TRUE),
      `DSD Dedupe` = `Deduplicated DSD Rollup` - `SUM - DSD`,
      `TA Dedupe` = `Deduplicated TA Rollup` - `SUM - TA`,
      `Crosswalk Dedupe` = `Total Deduplicated Rollup` - `SUM - Crosswalk Total`
    )


  d
}

#' testInvalidDedupeValues
#' @description Tests dedupe adjustments to determine if they are valid.
#' The function does not remove any data, but will flag any invalid dedupes
#' as an error.
#'
#' @param d Datapackr d object
#' @param header_cols header_cols object
#'
#' @return d object
testInvalidDedupeValues <- function(d, header_cols) {
  # TEST: Improper dedupe values
  #Flag them as an error but continue

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



  if (NROW(d$tests$dedupes_outside_range) > 0) {

    attr(d$tests$dedupes_outside_range, "test_name") <- "Dedupes Outside Acceptable Range"

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
        "ERROR! In tab PSNUxIM",
        ", DEDUPES OUTSIDE ACCEPTABLE RANGE: The following columns contain total",
        " deduplicated targets that are outside acceptable maximum/minimum ranges.",
        " (Your Data Pack notes these with red highlighting.) You must resolve",
        " these issues prior to DATIM import. ->  \n\t* ",
        paste(
          dedupe_issue_cols$col,
          collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
  }

  d

}

#' Title calculateFinalDedupeValues
#' @description Imputes zeros for dedupe values (if needed), keeps valid
#' depudes (where valid) and removes any dedupes which should not exist
#' based on the logic of pure and crosswalk deduplication.
#'
#' @param d Datapackr d object
#' @param header_cols header_cols object
#'
#' @return d object with final dedupe values
calculateFinalDedupeValues <- function(d, header_cols) {

  #Why do we pivot back to wide format here??
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
        TA_count == 0 | DSD_count == 0  ~ NA_real_,
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

  d

}

#' Title testNegativeTargetValues
#'
#' @description
#' Tests for any targets values which are negative. Converts any negative
#' target values to numeric NAs.
#'
#' @param d Datapackr d object
#' @param header_cols header_cols object
#'
#' @return Datapackr d object
testNegativeTargetValues <- function(d, header_cols) {

  cols_to_test <- grep("\\d{4,}_(DSD|TA)", names(d$data$SNUxIM), value = TRUE)

  #Nothing to test
  if (length(cols_to_test) == 0L) {
    return(d)
  }

  d$tests$negative_IM_targets <- d$data$SNUxIM %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(cols_to_test), ~ .x < 0))

  if (NROW(d$tests$negative_IM_targets) > 0) {

    #Get the names of the columns which match the regex which have negative values
    negative_cols <- d$tests$negative_IM_targets %>%
      dplyr::select(dplyr::matches("\\d{4,}_(DSD|TA)")) %>%
      dplyr::summarise_all(~ any(.x < 0))

    negative_col_names <- names(negative_cols)[unlist(negative_cols)]
    final_columns_to_keep <- c(header_cols$indicator_code, negative_col_names)
    d$tests$negative_IM_targets <- dplyr::select(d$tests$negative_IM_targets, dplyr::all_of(final_columns_to_keep))
    #Reshape this data to be more readable
    d$tests$negative_IM_targets <- tidyr::pivot_longer(d$tests$negative_IM_targets,
                                                      cols = tidyselect::all_of(negative_col_names),
                                                      names_to = "mechCode_supportType", values_to = "value")


    attr(d$tests$negative_IM_targets, "test_name") <- "Negative Mechanism Targets"

    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR!: In tab PSNUxIM,  ",
        NROW(d$tests$negative_IM_targets),
        " cases where negative numbers are being used for mechanism allocations.
        Please consult
        the validation report tab \"Negative Mechanism Targets\"
        for the specific rows where these values were found.\n\t* "
      )

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)

    d$data$SNUxIM %<>%
      dplyr::mutate(
        dplyr::across(
          dplyr::matches("\\d{4,}_(DSD|TA)"),
          ~ dplyr::if_else(.x < 0, NA_real_, .x))
      )

  }

  d
}

#' Title testInvalidPSNUs
#' @description
#' Tests for any PSNUs in the PSNUxIM sheet which are not valid.
#'
#' @param d Datapackr d object
#'
#' @return Datapackr d object
#'
testInvalidPSNUs <- function(d) {


  #Test for invalid PSNUs

  possible_psnus <- getValidOrgUnits(d$info$cop_year) %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    dplyr::rename(psnu_uid = uid) %>%
    dplyr::pull(psnu_uid)

  d$tests$invalid_psnus <- d$data$SNUxIM %>%
    dplyr::filter(!(psnuid %in% possible_psnus)) %>%
    dplyr::select(PSNU) %>%
    dplyr::distinct()

  if (NROW(d$tests$invalid_psnus) > 0) {

    attr(d$tests$invalid_psnus, "test_name") <- "Invalid PSNUs"
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR!: In tab PSNUxIM, ",
        NROW(d$tests$invalid_psnus),
        " invalid PSNU identifiers were detected. Please check the UID and fix the following PSNUs:",
        paste(d$tests$invalid_psnus$PSNU, sep = "", collapse = ";"))

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
  }

  d
}


#' Title testRoundDecimalValues
#' @description
#' Test for any values which are not whole integers. Using the round_trunc
#' function, values are rounded here.
#'
#' @param d Datapackr d object
#'
#' @return Datapackr d object
#'
testRoundDecimalValues <- function(d) {

  # TEST: Decimals; Error; Round ####
  d$tests$decimals <- d$data$SNUxIM %>%
    dplyr::filter(value %% 1 != 0)

  attr(d$tests$decimals, "test_name") <- "Decimal values"

  if (NROW(d$tests$decimals) > 0) {


    d$info$has_error <- FALSE

    warning_msg <-
      paste0(
        "WARNING! In tab PSNUxIM, ",
        NROW(d$tests$decimals),
        " DECIMAL VALUES found in the following columns! These will be rounded. -> \n\t* ",
        paste(unique(d$tests$decimals$mechCode_supportType), collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)
  }

  d$data$SNUxIM %<>%
    dplyr::mutate(value = round_trunc(value))

  d
}

#' Title testDropPositiveDedupe
#' @description
#' Identifies and deletes any cases where dedupe is positive in the PSNUxIM tab.
#'
#' @param d Datapackr d object
#'
#' @return Datapackr d object
testDropPositiveDedupe <- function(d) {

  # TEST: Positive Dedupes; Error; Drop ####
  d$tests$positive_dedupes <- d$data$SNUxIM %>%
    dplyr::filter(stringr::str_detect(mechCode_supportType, "Dedupe") & value > 0)

  attr(d$tests$positive_dedupes, "test_name") <- "Positive dedupes"

  if (NROW(d$tests$positive_dedupes) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR!: In tab PSNUxIM, ",
        NROW(d$tests$positive_dedupes),
        " cases where Deduplicated Rollups are greater than allowed maximum.",
        " You can find these by filtering to positive values in the `DSD Dedupe`, ",
        " `TA Dedupe`, and `Crosswalk Dedupe` columns (columns CX, CY, and CZ) in the PSNUxIM tab.",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
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

  d

}


#' Title generatePSNUxIMComparison
#' @description
#' Classifies discrepancies between the aggregated targets in the PSNUxIM
#' tab with those in the main sheets (original_targets).
#'
#'
#' @param d Datapackr d object
#' @param original_targets original_targets object
#'
#' @return Modified d object with a new object d$info$psnuxim_comparison

generatePSNUxIMComparison <- function(d, original_targets) {

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

  d$info$psnuxim_comparison <- d$data$SNUxIM %>%
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

  d
}

#' Title testRoundingDiffs
#' @description
#' Tests for rounding errors in the PSNUxIM allocation.
#'
#' @inheritParams datapackr_params
#'
#' @return Modified d object with a new object d$info$psnuxim_comparison
#' and if applicable, d$tests$PSNUxIM_rounding_diffs

testRoundingDiffs <- function(d) {

  if (is.null(d$info$psnuxim_comparison)) {
    stop("Could not find a existing PSNUxIM comparison")
  }

  d$tests$PSNUxIM_rounding_diffs <- d$info$psnuxim_comparison  %>%
    dplyr::filter(type == "Rounding") %>%
    dplyr::select(-type)

  attr(d$tests$PSNUxIM_rounding_diffs, "test_name") <- "PSNUxIM Rounding diffs"

  if (NROW(d$tests$PSNUxIM_rounding_diffs) > 0) {
    warning_msg <-
      paste0(
        "WARNING: In tab PSNUxIM, ",
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

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)
  }

  d
}

#' Title testImbalancedDistribution
#' @description
#' Tests for any imbalanced distribution between the main DataPack sheets
#' and the PSNUxIM tab.
#'
#' @param d Datapackr d object
#'
#' @return Datapackr d object
#'
testImbalancedDistribution <- function(d) {

  # TEST: Data Pack total not fully distributed to IM ####
  d$tests$imbalanced_distribution <- d$info$psnuxim_comparison %>%
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
        "WARNING!: In tab PSNUxIM, ",
        NROW(d$tests$imbalanced_distribution),
        " cases where distributed total across all mechanisms and Dedupe is",
        " either more or less than PSNU-level Target.",
        " To identify these, go to your PSNUxIM tab and filter the Rollup column",
        " to find cases where this is not equal to the Data Pack Target column (highlighted red).",
        " NOTE that this may be due to invalid mechanism names in row 14 of your PSNUxIM tab.",
        " For reference, this has affected the following indicators. -> \n\t* ",
        paste(imbalanced_distribution_inds, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)
    d$info$has_error <- TRUE
  }

  d
}

#' Title appendUnallocatedData
#'
#' @param d Datapackr d object
#' @description
#' Performs a test for any data which has not been allocated to a mechanism
#' in the PNSUxIM tab. If there is any unallocated data, it will be appended
#' to the already allocated data using the default mechanism.
#'
#' @return Datapackr d object
#'
appendUnallocatedData <- function(d) {

  # Add Unallocated Data to bottom ####
  d$tests$unallocatedIMs <- d$info$psnuxim_comparison %>%
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
        "ERROR!: In tab PSNUxIM, ",
        NROW(d$tests$unallocatedIMs),
        " cases where targets have not yet been fully allocated to IMs",
        " (excluding AGYW_PREV & cases possibly due to rounding). These data will be",
        " viewable alongside other data in the Data Pack Self-Service App & PAW",
        " Dossiers, but these cannot be imported into DATIM until fully",
        " allocated to IM.",
        " For reference, this has affected the following indicators. -> \n\t* ",
        paste(unallocated_inds, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR", d$info$tool)
    d$info$has_error <- TRUE
  }

  d
}
