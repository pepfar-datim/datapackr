#' @export
#' @importFrom magrittr %>% %<>%
#' @title Handle Prioritization data from a Data Pack
#'
#' @description Used to process the Prioritization sheet in a Data Pack
#' alerts when there are invalid or blank prioritizations.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'

handlePrioritization <- function(d, sheet) {
  
  # Remove _Military district from Prioritization extract as this can't be assigned a prioritization
  d$data$extract %<>% 
    dplyr::filter(!stringr::str_detect(PSNU, "^_Military"),
                  # Excuse valid NA Prioritizations
                  value != "NA")
  
  # blank prioritizations?
  blank_prioritizations <- d$data$extract %>% 
    dplyr::filter(is.na(value)) %>% 
    dplyr::select(PSNU)
  
  if (NROW(blank_prioritizations) > 0) {
    
    d$tests$blank_prioritizations <- blank_prioritizations
    attr(d$tests$blank_prioritizations, "test_name") <- "Blank prioritization levels"
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": MISSING PRIORITIZATIONS. Ensure a prioritization value is entered in each",
        " row of the column labeled 'SNU Prioritization' on the Prioritization tab.",
        " Refer to guidance on that tab and in the Data Pack User Guide to see",
        " appropriate entry options. You must enter a prioritization value for",
        " the following PSNUs -> \n\t* ",
        paste(blank_prioritizations$PSNU, collapse = "\n\t* "),
        "\n")
    
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    d$info$has_error <- TRUE
  }
  
  # Test for valid priortization values
  invalid_prioritizations <- d$data$extract %>%
    dplyr::filter(!(value %in% c("1", "2", "4", "5", "6", "7", "8")))
  
  if (NROW(invalid_prioritizations) > 0) {
    d$tests$invalid_prioritizations <- invalid_prioritizations
    attr(d$tests$invalid_prioritizations, "test_name") <- "Invalid prioritizations"
    
    invalid_prio_strings <- invalid_prioritizations %>%
      tidyr::unite(row_id, c(PSNU, value), sep = ":  ") %>%
      dplyr::arrange(row_id) %>%
      dplyr::pull(row_id)
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": INVALID PRIORITIZATIONS. The following Prioritizations are not valid for",
        " the listed PSNUs. Review the guidance on the Prioritization tab and in the",
        " Data Pack User Guide to understand valid prioritization options. Refer to those",
        " PSNUs flagged by this check and correct their validation values in the 'SNU Prioritization'",
        " column on the Prioritization tab. -> \n\t* ",
        paste(invalid_prio_strings, collapse = "\n\t* "),
        "\n")
    
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }
  return(d)
}
