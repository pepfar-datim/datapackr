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

  # Preamble ####
  if (d$info$tool != "OPU Data Pack") {
    stop("Cannot process that kind of tool. :(")
  }
  
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  sheet = "PSNUxIM"
  
  cols_to_keep <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  !is.na(indicator_code),
                  !indicator_code %in% c("12345_DSD","12345_TA"),
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
  
  # TODO: Check column structures ####
    # d <- checkColStructure(d, sheet)
  
  # Pare down to updated targets only ####
  d$data$extract <- 
    d$data$extract[c(cols_to_keep$col,(max(cols_to_keep$col)+1):NCOL(d$data$extract))]
  
  # TEST: Blank Col Names; Flag; Drop ####
  blank_col_headers <- names(d$data$extract)[which(nchar(names(d$data$extract))==0)]
  
  if (length(blank_col_headers) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", BLANK COLUMN HEADERS: The submission contains ",
        length(blank_col_headers),
        " columns with data, but no column header. For IM columns, please add a column header of the form 12345_DSD.",
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  d$data$extract <- d$data$extract[!(names(d$data$extract) %in% c(""))]
  
  # Drop columns with all NA ####
  d$data$extract %<>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::select_if(function(x) any(!is.na(x)))
  
  # TEST: Improper Col Names; Flag; Drop ####
  invalid_mech_headers <- names(d$data$extract) %>%
    tibble::tibble(col_name = .) %>%
    dplyr::filter(!col_name %in% cols_to_keep$indicator_code,
                  !(stringr::str_detect(col_name, "(\\d){4,6}")
                    & stringr::str_detect(col_name, "DSD|TA")))
  
  d$tests$invalid_mech_headers <- data.frame(invalid_mech_headers = invalid_mech_headers$col_name)
  attr(d$tests$invalid_mech_headers,"test_name") <- "Invalid mechanism headers"
  
  if (NROW(d$tests$invalid_mech_headers) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", INVALID COLUMN HEADERS: The following column headers are invalid and
        will be dropped in processing. Please use only the form 12345_DSD. ->  \n\t* ",
        paste(d$tests$invalid_mech_headers$invalid_mech_headers, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }

  d$data$extract %<>%
    dplyr::select(-dplyr::all_of(d$tests$invalid_mech_headers$invalid_mech_headers))
  
  # TEST: Duplicate Cols; Flag; Combine ####
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
  
  if (length(d$tests$duplicate_cols) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", DUPLICATE COLUMNS: The following columns appear to be duplicates and
        should be consolidated in your submission. Only the first appearance of
        these columns will be kept in processing. ->  \n\t* ",
        paste(d$tests$duplicate_cols, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  names(d$data$extract) <- col_names$col_name_new
      # --> This also removes non-essential text from IM name to leave only 12345_DSD format.
  
  # TEST: Non-numeric data; Flag; Convert & Drop ####
  non_numeric <- sapply(d$data$extract, function(x) stringr::str_extract(x, "[^[:digit:][:space:][:punct:]]+")) %>%
    as.data.frame() %>%
    dplyr::select(-dplyr::all_of(header_cols$indicator_code)) %>%
    dplyr::select_if(function(x) any(!is.na(x))) %>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.)))
  
  d$tests$non_numeric <- dplyr::bind_rows(d$tests$non_numeric, non_numeric)
  attr(d$tests$non_numeric,"test_name") <- "Non-numeric values"
  
  if(NROW(non_numeric) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": NON-NUMERIC VALUES found!",
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
    #sapply(d$data$extract, function(x) which(stringr::str_detect(x, "[^[:digit:][:space:][:punct:]]+")))

  d$data$extract %<>%
    {suppressWarnings(dplyr::mutate_at(., dplyr::vars(-dplyr::all_of(header_cols$indicator_code)),
                     as.numeric))
    }
  
  # Drop rows where entire row is NA ####
  d$data$extract %<>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.)))
  
  # TEST: No missing metadata ####
    #d <- checkMissingMetadata(d, sheet)
  
  # TEST: Missing Dedupe Rollup cols; Flag; Add ####
  dedupe_rollup_cols <- cols_to_keep %>%
    dplyr::filter(dataset == "mer" & col_type == "target") %>%
    dplyr::pull(indicator_code)
  
  missing_cols_fatal <- dedupe_rollup_cols[!dedupe_rollup_cols %in% names(d$data$extract)]
  
  d$tests$missing_cols_fatal <- data.frame(missing_cols_fatal = missing_cols_fatal)
  attr(d$tests$missing_cols_fatal, "test_name") <- "Fatally missing Dedupe Rollup columns"
  
  if (length(missing_cols_fatal) > 0) {
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", FATALLY MISSING COLUMNS: The following columns are missing, or have
        unexpected or blank column headers. Please check your submission. ->  \n\t* ",
        paste(missing_cols_fatal, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
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
  
  # TEST: Improper dedupe values; Flag; Continue ####
  
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
        "WARNING! In tab ",
        sheet,
        ", DEDUPES OUTSIDE ACCEPTABLE RANGE: The following columns contain
        total deduplicated targets that are outside acceptable maximum/minimum ranges.
        (The OPU Data Pack notes these with red highlighting.) You must resolve
        these issues prior to DATIM import. ->  \n\t* ",
        paste(
          dedupe_issue_cols$col,
          collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # TEST: Negative IM Targets; Flag; Drop ####
  d$tests$negative_IM_targets <- d$data$extract %>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(header_cols$indicator_code)) %>%
    dplyr::filter(stringr::str_detect(mechCode_supportType, "\\d{4,6}_(DSD|TA)")
                  & value < 0)
  attr(d$tests$negative_IM_targets,"test_name") <- "Negative Mechanism Targets"
  
  if (NROW(d$tests$negative_IM_targets) > 0) {
    warning_msg <- 
      paste0(
        "WARNING!: ",
        NROW(d$tests$negative_IM_targets),
        " cases where negative numbers are being used for mechanism allocations.",
        " The following mechanisms have been affected. These values will be dropped. -> \n\t* ",
        paste(unique(d$tests$negative_IM_targets$mechCode_supportType), collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
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
    
  # TEST: Decimals; Flag; Round ####
  d$tests$decimals <- d$data$extract %>%
    dplyr::filter(value %% 1 != 0)
  
  attr(d$tests$decimals,"test_name") <- "Decimal values"
  
  if (NROW(d$tests$decimals) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns! These will be rounded. -> \n\t* ",
        paste(unique(d$tests$decimals$mechCode_supportType), collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  d$data$extract %<>%
    dplyr::mutate(value = round_trunc(value))
  # Rename Dedupe IMs ####
  d$data$extract %<>%
    dplyr::mutate(
      mechCode_supportType = dplyr::case_when(
        mechCode_supportType == "DSD Dedupe" ~ "00000_DSD",
        mechCode_supportType == "TA Dedupe" ~ "00000_TA",
        mechCode_supportType == "Crosswalk Dedupe" ~ "00001_TA",
        TRUE ~ paste0(stringr::str_extract(mechCode_supportType, "\\d+"),
                      "_",
                      stringr::str_extract(mechCode_supportType, "DSD|TA"))
      )
    )
  
  # # TEST for positives against dedupes ####
  # d$tests$positive_dedupes <- d$data$extract %>%
  #   dplyr::filter(mech_code %in% c("00000", "00001") & value > 0)
  # attr(d$tests$positive_dedupes,"test_name") <- "Positive dedupes"
  # 
  # if (NROW(d$tests$positive_dedupes) > 0) {
  #   warning_msg <- 
  #     paste0(
  #       "WARNING!: ",
  #       NROW(d$tests$positive_dedupes),
  #       " cases where Deduplicated Rollups  positive numbers are being used for Dedupe allocations.",
  #       " You can find these by filtering on the Dedupe column in the PSNUxIM tab.")
  #   
  #   d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  # }
  # -> This is taken care of by testing for deduplication totals outside range.
  
  # TODO: TEST: Duplicate Rows; Flag; Combine ####
    d <- checkDuplicateRows(d, sheet)
  
  # TODO: TEST: Defunct disaggs; Flag; Drop ####
    #d <- defunctDisaggs(d, sheet)
  
  # Drop all zeros against IMs ####
  d$data$extract %<>%
    dplyr::filter(!(!stringr::str_detect(mechCode_supportType, "00000|00001")
                    & value == 0))
  
  # Drop unneeded Dedupes ####
  d$data$extract %<>%
    tidyr::pivot_wider
    
    # If only 1 DSD mechanism or only 1 TA mechanism (1 mech total):
      #   Do not import any dedupes
  
  
  
    # If only 1 DSD mech and only 1 TA mech (2 mechs total):
      #   Import Crosswalk Dedupe, whether 0 or <0
      #   Do not import any DSD or TA Dedupes
    
    # If >1 DSD mech, but no TA mechs (or vice versa):
      #   Import DSD or TA dedupes, whether 0 or <0
      #   Do not import any Crosswalk dedupes
    
    # If >1 DSD mech and >1 TA mech:
      #   Import all dedupes, whether 0 or <0

  # Drop NAs ####
  d$data$extract %<>% tidyr::drop_na(value)
  
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
