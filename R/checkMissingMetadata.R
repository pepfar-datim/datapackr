#' @export
#' @importFrom magrittr %>% %<>%
#' @title checkMissingMetadata(d)
#'
#' @description Checks data pulled from a single sheet in a Data Pack and
#' alerts where there are NAs instead of valid metadata.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#' 
#' @return d
#' 
checkMissingMetadata <- function(d, sheet) {
  if (sheet %in% c("SNU x IM","PSNUxIM")) {
    data = d$data$SNUxIM
  } else {
    data = d$data$extract
  }
  
  missing_metadata <- data %>%
    dplyr::mutate(row = (1:dplyr::n())) %>%
    dplyr::filter(is.na(PSNU) | is.na(indicator_code) | is.na(ID))
  
  d[["tests"]][["missing_metadata"]][[as.character(sheet)]] <- missing_metadata
  
  # Alert to missing metadata
  if (NROW(missing_metadata) > 0) {
    
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", MISSING PSNU, INDICATOR_CODE, OR ID: ",
        NROW(missing_metadata),
        " rows where blank entries exist in the PSNU, indicator_code, or ID columns.",
        " Note that blank entries in these columns will prevent processing of",
        " data in that row. The following rows are affected:\n\t* ",
        paste(missing_metadata$row, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  return(d)
  
}
