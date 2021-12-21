#' @export
#' @importFrom magrittr %>% %<>%
#' @title Checks decimal values in a specific Data Pack sheet
#'
#' @description Checks data pulled from a single sheet in a Data Pack for decimal values
#' alerts where there are invalid decimal values.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'

checkDecimalValues <- function(d, sheet) {
  
  # TEST for Decimal values
  decimals_allowed <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & col_type == "target"
                  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% unique(d$data$extract$indicator_code)
                  & value_type == "percentage") %>%
    dplyr::pull(indicator_code)
  
  decimal_cols <- d$data$extract %>%
    dplyr::filter(value %% 1 != 0
                  & !indicator_code %in% decimals_allowed) %>%
    dplyr::rename(sheet = sheet_name)
  
  d$tests$decimal_values <- dplyr::bind_rows(d$tests$decimal_cols, decimal_cols)
  attr(d$tests$decimal_values, "test_name") <- "Decimal values"
  
  if (NROW(decimal_cols) > 0) {
    
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns! Ensure all values entered",
        " against FY22 Targets are whole, positive, numeric values. (The only exception",
        " to this rule may be HIV_PREV.) These will be rounded. -> \n\t* ",
        paste(unique(decimal_cols$indicator_code), collapse = "\n\t* "),
        "\n")
    
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }
  
  return(d)
}
