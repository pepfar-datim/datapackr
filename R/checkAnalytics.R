#' @export
#' @title Check Data Pack data for retention < 98%
#'
#' @description Check data gathered from Data Pack to identify cases where
#' retention is less than the standard of 98%.
#'
#' @param data Analytics object to analyze
#' @param msg Warning message to append issues to
#' 
#' @return a
#' 
analyze_retention <- function(data, msg = NULL) {
  a <- NULL
  
  data %<>%
    dplyr::mutate(
      TX.Retention.T = 
        (TX_CURR.N.Age_Sex_HIVStatus.T)
      / (TX_CURR.N.Age_Sex_HIVStatus.T_1 + TX_NEW.N.Age_Sex_HIVStatus.T)
    )
  
  retention_issues <- data %>%
    dplyr::filter(TX.Retention.T < 0.98) %>%
    dplyr::select(
      PSNU, psnuid, Age, Sex, KeyPop,
      TX.Retention.T,
      TX_CURR.N.Age_Sex_HIVStatus.T,
      TX_CURR.N.Age_Sex_HIVStatus.T_1,
      TX_NEW.N.Age_Sex_HIVStatus.T)
  
  if (NROW(retention_issues) > 0 ) {
    
    a$test_results <- retention_issues
    attr(a$test_results, "test_name") <- "Retention rate issues"
    
    national_avg_ret <- data %>%
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
            length(unique(data$psnuid)))),
        " PSNUs affected.\n\n\t* ",
        "Lowest retention rate observed: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(retention_issues$TX.Retention.T))),
        "\n\n\t* ",
        "National average retention rate: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(national_avg_ret$TX.Retention.T))),
        "\n")
    
    a$msg <- append(msg, analytics_warning_msg)
    
  } 
  
  return(a)
}


#' @export
#' @title Check Data Pack data for linkage < 95%
#'
#' @description Check data gathered from Data Pack to identify cases where
#' linkage rates are less than the standard of 95%.
#'
#' @param data Analytics object to analyze
#' @param msg Warning message to append issues to
#' 
#' @return a
#' 
analyze_linkage <- function(data, msg = NULL) {
  a <- NULL
  
  pmtct_hei_pos_data <- 
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = "PMTCT_EID",
      range = readxl::cell_limits(
        c(headerRow(tool = "Data Pack", cop_year = d$info$cop_year), 1),
        c(NA, NA)),
      .name_repair = "minimal"
    )
  
  duplicate_cols <- duplicated(names(pmtct_hei_pos_data))
  
  if (any(duplicate_cols)) {
    pmtct_hei_pos_data <- pmtct_hei_pos_data[,-which(duplicate_cols)]
  }
  
  pmtct_hei_pos_data %<>%
    dplyr::select(PSNU, PMTCT_HEI_POS.N.infected)  %<>%
    dplyr::filter(complete.cases(.)) %>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"),
      Age = "<01"
    )
  
  tx_new_lt1 <- analytics_dataset %>%
    dplyr::select(PSNU, psnuid, Age, Sex, KeyPop, TX_NEW.N.Age_Sex_HIVStatus.T) %>%
    dplyr::filter(Age == "<01") %>%
    dplyr::mutate(Sex = NA_character_) %>%
    dplyr::group_by_at(dplyr::vars(-TX_NEW.N.Age_Sex_HIVStatus.T)) %>%
    dplyr::summarise(
      TX_NEW.N.Age_Sex_HIVStatus.T = sum(TX_NEW.N.Age_Sex_HIVStatus.T))
  
  linkage_lt1 <- tx_new_lt1 %>%
    dplyr::full_join(pmtct_hei_pos_data,
                     by = c("PSNU", "psnuid", "Age", "Sex", "KeyPop")) %>%
    dplyr::mutate_at(dplyr::vars(-PSNU,-psnuid,-Age,-Sex,-KeyPop),
                     tidyr::replace_na, 0)
  
  sj <- analytics_dataset %>%
    dplyr::mutate(PMTCT_HEI_POS.N.infected = 0) %>%
    dplyr::bind_rows(linkage_lt1) %>%
    dplyr::mutate_at(dplyr::vars(-PSNU,-psnuid,-Age,-Sex,-KeyPop),
                     tidyr::replace_na, 0) %>%
    dplyr::mutate(
      HTS_TST_POS.T =
        HTS_INDEX_COM.N.Age_Sex_Result.T.NewPos
      + HTS_INDEX_FAC.N.Age_Sex_Result.T.NewPos
      + HTS_TST_EmergencyWard.N.Age_Sex_Result.T.Positive
      + HTS_TST_Inpat.N.Age_Sex_Result.T.Positive
      + HTS_TST_Malnutrition.N.Age_Sex_Result.T.Positive
      + HTS_TST_MobileMod.N.Age_Sex_Result.T.Positive
      + HTS_TST_OtherMod.N.Age_Sex_Result.T.Positive
      + HTS_TST_OtherPITC.N.Age_Sex_Result.T.Positive
      + HTS_TST_Pediatric.N.Age_Sex_Result.T.Positive
      + HTS_TST_PMTCTPostANC1.N.Age_Sex_Result.T.Positive
      + HTS_TST_STIClinic.N.Age_Sex_Result.T.Positive
      + HTS_TST_VCT.N.Age_Sex_Result.T.Positive
      + HTS_TST.N.KeyPop_Result.T.Positive
      + PMTCT_STAT.N.Age_Sex_KnownNewResult.T.NewPos
      + TB_STAT.N.Age_Sex_KnownNewPosNeg.T.NewPos
      + VMMC_CIRC.N.Age_Sex_HIVStatus.T.Positive
      + PMTCT_HEI_POS.N.infected,
      HTS_TST.Linkage.T = 
        dplyr::case_when(
          HTS_TST_POS.T == 0
          & TX_NEW.N.Age_Sex_HIVStatus.T == 0
          & TX_NEW.N.KeyPop_HIVStatus.T == 0
          ~ NA_real_,
          TRUE ~ 
            (TX_NEW.N.Age_Sex_HIVStatus.T + TX_NEW.N.KeyPop_HIVStatus.T)
          / (HTS_TST_POS.T)
        )
    )
  
  if (NROW(linkage_issues) > 0 ) {
    
    a$test_results <- linkage_issues
    attr(a$test_results, "test_name") <- "Linkage rate issues"
    
    national_avg_linkage <- data %>%
      # dplyr::select(
      #   TX_CURR.N.Age_Sex_HIVStatus.T,
      #   TX_CURR.N.Age_Sex_HIVStatus.T_1,
      #   TX_NEW.N.Age_Sex_HIVStatus.T) %>%
      # dplyr::summarise(
      #   TX_CURR.N.Age_Sex_HIVStatus.T = sum(TX_CURR.N.Age_Sex_HIVStatus.T, na.rm = T),
      #   TX_CURR.N.Age_Sex_HIVStatus.T_1 = sum(TX_CURR.N.Age_Sex_HIVStatus.T_1, na.rm = T),
      #   TX_NEW.N.Age_Sex_HIVStatus.T = sum(TX_NEW.N.Age_Sex_HIVStatus.T, na.rm = T)) %>%
      # dplyr::mutate(
      #   TX.Retention.T = 
      #     (TX_CURR.N.Age_Sex_HIVStatus.T)
      #   / (TX_CURR.N.Age_Sex_HIVStatus.T_1 + TX_NEW.N.Age_Sex_HIVStatus.T)
      # )
    
    analytics_warning_msg <- 
      paste0(
        "WARNING! LINKAGE RATES < 95%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(linkage_issues$psnuid)), " of ",
            length(unique(data$psnuid)))),
        " PSNUs affected.\n\n\t* ",
        "Lowest linkage rate observed: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(linkage_issues$HTS_TST.Linkage.T))),
        "\n\n\t* ",
        "National average linkage rate: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(national_avg_linkage$HTS_TST.Linkage.T))),
        "\n")
    
    a$msg <- append(msg, analytics_warning_msg)
    
  } 
  
  return(a)
  
}




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
                       values_from = value) %>%
    dplyr::mutate_at(dplyr::vars(-PSNU,-psnuid,-Age,-Sex,-KeyPop),
                     tidyr::replace_na, 0)
  
  # TEST: Retention Rates ####
  a <- analyze_retention(data = analytics_dataset,
                         msg = d$info$analytics_warning_msg)
  
  if (!is.null(a)) {
    d$info$analytics_warning_msg <- a$msg
    d$tests$retention <- a$test_results
  }
  
  # TEST: Linkage rates ####
  a <- analyze_linkage(data = analytics_dataset,
                         msg = d$info$analytics_warning_msg)
  
  if (!is.null(a)) {
    d$info$analytics_warning_msg <- a$msg
    d$tests$linkage <- a$test_results
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
