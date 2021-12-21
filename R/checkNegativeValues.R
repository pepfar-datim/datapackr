#' @export
#' @importFrom magrittr %>% %<>%
#' @title Check a Data Pack sheet for negative values
#'
#' @description Checks for negative values in a specific Data Pack sheet
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'

checkNegativeValues <- function(d, sheet) {
  negative_values <- d$data$extract %>%
    dplyr::filter(value < 0)
  
  d$tests$negative_values <- dplyr::bind_rows(d$test$negative_values, negative_values)
  attr(d$tests$negative_values, "test_name") <- "Negative values"
  
  if (NROW(negative_values) > 0) {
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
        " against FY22 Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
        paste(unique(d$tests$negative_values$indicator_code), collapse = "\n\t* "),
        "\n")
    
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }
  return(d)
}