#' @export
#' @title Check Data Pack data for analytics concerns
#'
#' @description Check data gathered from Data Pack to identify validation concerns
#' at the PSNU level.
#'
#' @param d Datapackr object.
#' 
#' @return d
#' 
checkAnalytics <- function(d,
                           model_data_path) {
  
  # Start running log of all warning and information messages ####
  d$keychain$model_data_path <- model_data_path
  d$info$analytics_warning_msg <- NULL
  d$info$has_analytics_error <- FALSE
  
  # Pull in PSNU-level data ####
  PSNU <- d$data$MER %>%
    dplyr::bind_rows(d$data$SUBNAT_IMPATT) %>%
    dplyr::select(-sheet_name)
  
  # Prepare model data ####
  model_data <- readRDS(d$keychain$model_data_path)
  
  if (!all(d$info$country_uids %in% names(model_data))) {
    missing <- country_uids[!d$info$country_uids %in% names(model_data)]
    analytics_warning_msg <- 
      paste0(
        "Model data file does not have data for the following country_uids: \r\n\t* ",
        paste(missing, collapse = "\r\n\t* ")
      )
    
    d$info$analytics_warning_msg <- append(d$info$analytics_warning_msg, analytics_warning_msg)
  }
  
  valid_cos <- datapackr::valid_category_options %>%
    dplyr::select(id, datapack_disagg) %>%
    dplyr::distinct()
  
  model_data <- model_data[d$info$country_uids] %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na(value) %>%
    dplyr::left_join(
      datapackr::valid_PSNUs %>%
        dplyr::filter(country_uid %in% d$info$country_uids) %>%
        add_dp_psnu(.) %>%
        dplyr::select(PSNU = dp_psnu, psnu_uid),
      by = c("psnu_uid" = "psnu_uid")
    ) %>%
    dplyr::left_join(dplyr::rename(valid_cos, Age = datapack_disagg),
                     by = c("age_option_uid" = "id")) %>%
    dplyr::left_join(dplyr::rename(valid_cos, Sex = datapack_disagg),
                     by = c("sex_option_uid" = "id")) %>%
    dplyr::left_join(dplyr::rename(valid_cos, KeyPop = datapack_disagg),
                     by = c("kp_option_uid" = "id")) %>%
    dplyr::select(PSNU, psnuid = psnu_uid, indicator_code, Age,
                  Sex, KeyPop, value)
  
  # Add model_data to PSNU-level dataset ####
  analytics_dataset <- PSNU %>%
    dplyr::bind_rows(model_data) %>%
    dplyr::arrange(indicator_code, PSNU, Age, Sex, KeyPop) %>%
    tidyr::pivot_wider(names_from = indicator_code,
                       values_from = value)
  
  # TEST: Retention Rates ####
  analytics_dataset %<>%
    dplyr::mutate(
      TX.Retention.T = 
        (TX_CURR.N.Age_Sex_HIVStatus.T)
            / (TX_CURR.N.Age_Sex_HIVStatus.T_1 + TX_NEW.N.Age_Sex_HIVStatus.T)
    )
  
  retention_issues <- analytics_dataset %>%
    dplyr::filter(TX.Retention.T < 0.98) %>%
    dplyr::select(
      PSNU, psnuid, Age, Sex, KeyPop,
      TX.Retention.T,
      TX_CURR.N.Age_Sex_HIVStatus.T,
      TX_CURR.N.Age_Sex_HIVStatus.T_1,
      TX_NEW.N.Age_Sex_HIVStatus.T)
  
  if (NROW(retention_issues) > 0 ) {
    
    national_avg_ret <- analytics_dataset %>%
      dplyr::select(
        TX_CURR.N.Age_Sex_HIVStatus.T,
        TX_CURR.N.Age_Sex_HIVStatus.T_1,
        TX_NEW.N.Age_Sex_HIVStatus.T) %>%
      dplyr::summarise(
        TX_CURR.N.Age_Sex_HIVStatus.T = sum(TX_CURR.N.Age_Sex_HIVStatus.T, na.rm = T),
        TX_CURR.N.Age_Sex_HIVStatus.T_1 = sum(TX_CURR.N.Age_Sex_HIVStatus.T_1, na.rm = T),
        TX_NEW.N.Age_Sex_HIVStatus.T = sum(TX_NEW.N.Age_Sex_HIVStatus.T, na.rm = T)) %>%
      dplyr::mutate(
        TX.Retention.T = 
          (TX_CURR.N.Age_Sex_HIVStatus.T)
        / (TX_CURR.N.Age_Sex_HIVStatus.T_1 + TX_NEW.N.Age_Sex_HIVStatus.T)
      )
    
    analytics_warning_msg <- 
      paste0(
        "WARNING! RETENTION RATES < 98%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(retention_issues$psnuid)), " of ",
            length(unique(analytics_dataset$psnuid)))),
        " PSNUs affected.\n\n\t* ",
        "Lowest retention rate observed: ",
        crayon::bold(sprintf("%.1f%%", 100*min(retention_issues$TX.Retention.T))),
        "\n\n\t* ",
        "National average retention rate: ",
        crayon::bold(sprintf("%.1f%%", 100*min(national_avg_ret$TX.Retention.T))),
        "\n")
    
    d$info$analytics_warning_msg <- append(d$info$analytics_warning_msg, analytics_warning_msg)
    
  }
  
  # If warnings, show all grouped by sheet and issue ####
  if (!is.null(d$info$analytics_warning_msg) & interactive()) {
    options(warning.length = 8170)
    
    messages <-
      paste(
        paste(
          seq_along(d$info$analytics_warning_msg),
          ": " , d$info$analytics_warning_msg
          #stringr::str_squish(gsub("\n", "", d$info$analytics_warning_msg))
        ),
        sep = "",
        collapse = "\r\n")
    
    key = paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing your tool.\r\n",
      "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
      "*********************\r\n\r\n")
    
    cat(crayon::red(crayon::bold("ANALYTICS ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }
  
  return(d)  
}
