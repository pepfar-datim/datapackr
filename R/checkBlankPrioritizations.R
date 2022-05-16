#' @export
#' @title checkInvalidPrioritizations(d, sheet)
#'
#' @description Checks if there are invalid and missing prioritizations.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'

checkBlankPrioritizations <- function(d, sheet, quiet = T) {
  
  # Get data ----
  data <- d$sheets[[sheet]]
  
  # mung ----
  
  # List Target Columns ####
  target_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & (col_type == "target" | (col_type == "result" & dataset == "subnat"))
                  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(data)) %>%
    dplyr::pull(indicator_code)
  
  # in certain cases target cols are not present in which case kick back the d object
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
  
  #TEST: actual test begins ----
  
  # Remove _Military district from Prioritization extract as this can't be assigned a prioritization ####
  data <- data %>%
    dplyr::filter(!stringr::str_detect(PSNU, "^_Military"))
  
  blank_prioritizations <- data %>%
    dplyr::filter(is.na(value)) %>%
    dplyr::select(PSNU)
  
  if (NROW(blank_prioritizations) > 0) {
    
    lvl <- "ERROR"
    
    msg <-
      paste0(
        lvl,"! In tab ",
        sheet,
        ": MISSING PRIORITIZATIONS. Ensure a prioritization value is entered in each",
        " row of the column labeled 'SNU Prioritization' on the Prioritization tab.",
        " Refer to guidance on that tab and in the Data Pack User Guide to see",
        " appropriate entry options. You must enter a prioritization value for",
        " the following PSNUs -> \n\t* ",
        paste(blank_prioritizations$PSNU, collapse = "\n\t* "),
        "\n")
    
    d$tests$blank_prioritizations <- blank_prioritizations
    attr(d$tests$blank_prioritizations, "test_name") <- "Blank prioritization levels"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    
  }
  
  return(d)
  
}