#' @export
#' @title checkInvalidOrgUnits(d)
#'
#' @description Checks data pulled from a single sheet in a Data Pack and
#' alerts where there are unallowed org units based on current DATIM PSNU level.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'
checkInvalidOrgUnits <- function(d, sheet, quiet = T) {
  
  if (!quiet) {
    messages <- MessageQueue()
  }

  # get data ----
  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {
    
    data <- d$sheets[["PSNUxIM"]]
  } else {
    data <- d$sheets[[as.character(sheet)]]
  }

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
  
  # TEST: actual test begins ----
  invalid_orgunits <- data %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("value|distribution")), dplyr::any_vars(!is.na(.))) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("value|distribution")), dplyr::any_vars(. != 0)) %>%
    dplyr::select(PSNU) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")) %>%
    dplyr::left_join(datapackr::valid_PSNUs, by = c("psnuid" = "psnu_uid")) %>%
    dplyr::filter(is.na(psnu)
                  | is.na(psnuid)) %>%
    dplyr::select(PSNU) %>%
    dplyr::mutate(sheet = sheet)
  
  if (NROW(invalid_orgunits) > 0) {
    
    lvl <- "ERROR"

    warning_msg <-
      paste0(
        lvl, "! In tab ",
        sheet,
        ", INVALID ORG UNITS: Please review all tabs flagged by this test to correct",
        " any incorrectly added or edited organization units in the PSNU/DSNU column.",
        " If you believe this is in error, please also confirm in DATIM that all flagged",
        " organization units are correctly added and valid. The following org units are",
        " not valid PSNUs, or do not contain the required DATIM PSNU UID ->  \n\t* ",
        paste(invalid_orgunits$PSNU, collapse = "\n\t* "),
        "\n")

    d$tests$invalid_orgunits <- dplyr::bind_rows(d$tests$invalid_orgunits, invalid_orgunits)
    attr(d$tests$invalid_orgunits, "test_name") <- "Invalid orgunits"
    d$info$messages <- appendMessage(d$info$messages, warning_msg, lvl)
    
    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
    
  }
  
  if (!quiet) {
    printMessages(messages)
  }

  return(d)
}
