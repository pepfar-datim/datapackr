#' @export
#' @title Check for invalid Indicator Codes
#'
#' @description
#' Check for invalid Indicator Codes
#'
#' @param d Datapackr object
#'
#' @return d
#'
checkInvalidIndicatorCodes <- function(d) {
  #Test any invalid indicator codes

  indicator_codes_sheet <- d$data$extract %>%
    dplyr::select(indicator_code) %>%
    dplyr::distinct()


    indicator_codes_schema <- pick_schema(cop_year = d$info$cop_year,
                                          tool = d$info$tool) %>%
      dplyr::filter(dataset == "mer", col_type == "target") %>%
      dplyr::select(indicator_code) %>%
      dplyr::mutate(is_valid = TRUE)


  invalid_indicator_codes <- dplyr::left_join(indicator_codes_sheet,
                                              indicator_codes_schema) %>%
    dplyr::filter(is.na(is_valid))

  d$tests$invalid_indicator_codes <- invalid_indicator_codes
  attr(d$tests$invalid_indicator_codes, "test_name") <-
    "Invalid indicator codes"

  if (NROW(d$tests$invalid_indicator_codes) > 0) {
    d$info$has_error <- TRUE

    warning_msg <-
      paste0(
        "ERROR! INVALID INDICATOR CODES: The following indicator codes are invalid",
        " will be dropped in processing. ->  \n\t* ",
        paste(d$tests$invalid_indicator_codes$indicator_code, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  }
  return(d)
}
