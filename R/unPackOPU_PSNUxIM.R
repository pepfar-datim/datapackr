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

  if (d$info$tool != "OPU Data Pack") {
    stop("Cannot process that kind of tool. :(")
  }
  
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  sheet = "PSNUxIM"
  
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
  # TODO: Make this more dynamic
  d$data$extract <- d$data$extract[c(1:5,90:NCOL(d$data$extract))]
  
  # TODO: Test for non-numeric data ####
  
  
  # Convert to numeric data types ####
  d$data$extract %<>%
    dplyr::mutate_at(dplyr::vars(-PSNU, -indicator_code, -Age, -Sex, -KeyPop),
                     as.numeric)
    
  # Recalculate dedupes ####
    ## Other than IM cols, only the following should be considered safe for reuse here:
      # - Deduplicated DSD Rollup
      # - Deduplicated TA Rollup
      # - Total Deduplicated Rollup
    ## All others must be recalculated to protect against formula breakers. 
  
  d$data$extract %<>%
    dplyr::mutate(
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{5,}_DSD")), na.rm = TRUE),
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{5,}_TA")), na.rm = TRUE),
      `DSD Dedupe` = `Deduplicated DSD Rollup` - `DSD Duplicated Rollup`,
      `TA Dedupe` = `Deduplicated TA Rollup` - `TA Duplicated Rollup`,
      `Max Total Deduplicated Rollup` =
        rowSums(dplyr::select(.,
                              `Deduplicated DSD Rollup`, `Deduplicated TA Rollup`),
                na.rm = TRUE),
      `Crosswalk Dedupe` = `Total Deduplicated Rollup` - `Max Total Deduplicated Rollup`
    )
  
  # Remove duplicate columns (Take the first example) ####
  duplicate_cols <- duplicated(names(d$data$extract))
  
  if (any(duplicate_cols)) {
    d$data$extract <- d$data$extract[,-which(duplicate_cols)]
  }
  
  # Make sure no blank column names ####
  d$data$extract %<>%
    tibble::as_tibble(.name_repair = "unique") %>%
  
  # Drop rows where entire row is NA ####
    dplyr::filter_all(dplyr::any_vars(!is.na(.)))
  
  # TEST: No missing metadata ####
  d <- checkMissingMetadata(d, sheet)
  
  # If PSNU has been deleted, drop the row ####
  d$data$extract %<>%
    dplyr::filter(!is.na(PSNU))
  
  # Check for Formula changes ####
    # TODO
    
  # TEST Column headers for appropriate structure ####
  expected_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  !is.na(indicator_code),
                  !indicator_code %in% c("12345_DSD","12345_TA")) %>%
    dplyr::pull(indicator_code) %>% 
    unique(.)
  
  invalid_mech_headers <- d$data$extract %>%
    dplyr::select(-dplyr::one_of(expected_cols)) %>%
    dplyr::select(-dplyr::matches("(\\d){4,6}_(DSD|TA)")) %>%
    names()
  
  d$tests$invalid_mech_headers <- data.frame(invalid_mech_headers = invalid_mech_headers )
  attr(d$tests$invalid_mech_headers,"test_name") <- "Invalid mechanism headers"
  
  if (length(invalid_mech_headers) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", INVALID COLUMN HEADERS: The following column headers are invalid and
        will be dropped in processing. Please use only the form 12345_DSD. ->  \n\t* ",
        paste(invalid_mech_headers, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Extract PSNU uid ####
  d$data$extract %<>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")
    ) %>%
    dplyr::select(PSNU, psnuid, Age, Sex, KeyPop,
                  dplyr::everything())
  
  # Remove all unneeded columns ####
  # TODO: make this more dynamic, keyed off schema
  d$data$extract %<>%
    dplyr::select(
      -`Max Total Deduplicated Rollup`, -`Min Total Deduplicated Rollup`,
      -`Total Deduplicated Rollup`, -`Max Deduplicated DSD Rollup`,
      -`Min Deduplicated DSD Rollup`, -`Deduplicated DSD Rollup`,
      -`Max Deduplicated TA Rollup`, -`Min Deduplicated TA Rollup`,
      -`Deduplicated TA Rollup`, -`Total Duplicated Rollup`,
      -`DSD Duplicated Rollup`, -`TA Duplicated Rollup`
    )
    
  # Gather all values in single column ####
  d$data$extract %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -PSNU, -indicator_code, -psnuid, -Age, -Sex, -KeyPop) %>%
    dplyr::select(PSNU, psnuid, indicator_code, Age, Sex, KeyPop,
                  mechCode_supportType, value)
    
  # Rename Dedupe IMs ####
    dplyr::mutate(
      mechCode_supportType = dplyr::case_when(
        mechCode_supportType == "DSD Dedupe" ~ "00000_DSD",
        mechCode_supportType == "TA Dedupe" ~ "00000_TA",
        mechCode_supportType == "Crosswalk Dedupe" ~ "00001_TA",
        TRUE ~ mechCode_supportType
      )
    ) %>%
  
  # Get mech codes and support types ####
    dplyr::mutate(
      mechanism_code = stringr::str_extract(mechCode_supportType, "(\\d{4,6})"),
      support_type = stringr::str_extract(mechCode_supportType, "(?<=_)DSD|TA")) %>%
    dplyr::select(PSNU, psnuid,  indicator_code, Age, Sex,
                  KeyPop, mechanism_code, support_type, value)
  
  # TEST for positives against dedupes ####
  d$tests$invalid_dedupes <- d$data$extract %>%
    dplyr::filter(mechanism_code %in% c("00000", "00001") & value > 0)
  attr(d$tests$invalid_dedupes,"test_name") <- "Invalid dedupes"
  
  if (NROW(d$tests$invalid_dedupes) > 0) {
    warning_msg <- 
      paste0(
        "WARNING!: ",
        NROW(d$tests$invalid_dedupes),
        " cases where positive numbers are being used for Dedupe allocations.",
        " You can find these by filtering on the Dedupe column in the PSNUxIM tab.")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # TEST for negatives against non-dedupes ####
  d$tests$negative_IM_targets <- d$data$extract %>%
    dplyr::filter(!mechanism_code %in% c("00000", "00001") & value < 0)
  attr(d$tests$negative_IM_targets,"test_name") <- "Negative Mechanism targets"
  
  if (NROW(d$tests$negative_IM_targets) > 0) {
    warning_msg <- 
      paste0(
        "WARNING!: ",
        NROW(d$tests$negative_IM_targets),
        " cases where negative numbers are being used for mechanism allocations.",
        " The following mechanisms have been affected. -> \n\t* ",
        paste(unique(d$tests$negative_IM_targets$mechanism_code), collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # TEST for Decimal values ####
  d$tests$decimal_values <- d$data$extract %>%
    dplyr::filter(value %% 1 != 0)
  
  attr(d$tests$decimal_values,"test_name") <- "Decimal values"
  
  if (NROW(d$tests$decimal_values) > 0) {
    
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns! These will be rounded. -> \n\t* ",
        paste(unique(d$tests$decimal_values$indicator_code), collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Round decimals to integers ####
  d$data$extract %<>%
    dplyr::mutate(value = round_trunc(value)) %>%
    
  # Drop NAs
    tidyr::drop_na(value)
  
  # TODO: TEST for duplicate rows ####
  #d <- checkDuplicateRows(d, sheet)
  
  # TODO: TEST for defunct disaggs ####
  #d <- defunctDisaggs(d, sheet)
  
  # TEST for invalid DSD TA ####
  d$tests$invalid_DSDTA <- d$data$extract %>%
    dplyr::filter(is.na(support_type))
  attr(d$tests$invalid_DSDTA,"test_name") <- "Invalid DSD/TA"
  
  if (NROW(d$tests$invalid_DSDTA) > 0) {
    warning_msg <- 
      paste0(
        "WARNING!: ",
        NROW(d$tests$invalid_DSDTA),
        " cases where column headers in row 14 of your PSNUxIM tab have prevented",
        " us from determining whether you intended data to be distributed to DSD or TA.",
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Drop unneeded Dedupes:
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
  
  
  # Make sure Dedupes go in after values ####
  
  
  
  return(d)
  
}
