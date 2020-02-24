#' @export
#' @title Check Data Pack data for analytics concerns
#'
#' @description Check data gathered from Data Pack to identify validation concerns
#'
#' @param d Datapackr object.
#' 
#' @return d
#' 
checkAnalytics <- function(d) {
  
  d$analytics$PSNU <- d$data$MER %>%
    dplyr::bind_rows(d$data$SUBNAT_IMPATT)
  
  if (d$info$has_psnuxim) {
    d$analytics$PSNUxIM <- d$data$distributedMER
    
    # TODO: How to add SUBNAT_IMPATT to this dataset
  }
  
  # Add in past data from model
  
  # Prepare data ####
  if (!all(country_uids %in% names(model_data))) {
    missing <- country_uids[!country_uids %in% names(model_data)]
    stop(
      paste0(
        "Model data file does not have data for the following country_uids: \r\n\t- ",
        paste(missing, collapse = "\r\n\t- ")
      )
    )
  }
  
  data <- model_data[country_uids] %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na(value) %>%
    dplyr::select(-period)
  
  
  
  
  d$analytics$PSNU %<>%
    dplyr::select(-sheet_name) %>%
    tidyr::pivot_wider(names_from = indicator_code,
                       values_from = value)
  
  # Test Retention Rates
  
  
  
  # If warnings, show all grouped by sheet and issue
  if (!is.null(d$info$analytics_warning_msg) & interactive()) {
    options(warning.length = 8170)
    
    messages <-
      paste(
        paste(
          seq_along(d$info$warning_msg),
          ": " , d$info$warning_msg
          #stringr::str_squish(gsub("\n", "", d$info$warning_msg))
        ),
        sep = "",
        collapse = "\r\n")
    
    key = paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing your tool.\r\n",
      "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
      "*********************\r\n\r\n")
    
    cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }
  
  return(d)  
}
