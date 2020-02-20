#' @export
#' @importFrom magrittr %>% %<>%
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
checkInvalidOrgUnits <- function(d, sheet) {
  if (sheet %in% c("SNU x IM","PSNUxIM")) {
    data = d$data$SNUxIM
  } else {
    data = d$data$extract
  }
  
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
    dplyr::pull(PSNU)
  
  if (length(invalid_orgunits) > 0) {
    
    d[["tests"]][["invalid_orgunits"]][[as.character(sheet)]] <- invalid_orgunits
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", INVALID ORG UNITS: The following org units are not valid PSNUs, or do not",
        " contain the required DATIM PSNU UID ->  \n\t* ",
        paste(invalid_orgunits, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
 
  return(d) 
}
  