#' @export
#' @title Check Data Pack data for retention < 98\% or >100\%
#'
#' @description Check data gathered from Data Pack to identify cases where
#' retention is less than the standard of 98\% or >100\%.
#'
#' @param data Analytics object to analyze
#' 
#' @return a
#' 
analyze_retention <- function(data) {
  a <- NULL
  
  data %<>%
    dplyr::mutate(
      TX.Retention.T = 
        (TX_CURR.N.Age_Sex_HIVStatus.T)
      / (TX_CURR.N.Age_Sex_HIVStatus.T_1 + TX_NEW.N.Age_Sex_HIVStatus.T)
    )
  
  retention_issues <- data %>%
    dplyr::filter(TX.Retention.T < 0.98 | TX.Retention.T > 1) %>%
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
    
    a$msg <- 
      paste0(
        "WARNING! RETENTION RATES <98% OR >100%: \n\n\t* ",
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
    
  } 
  
  return(a)
}


#' @export
#' @title Check Data Pack data for linkage < 95\% or >100\%
#'
#' @description Check data gathered from Data Pack to identify cases where
#' linkage rates are less than the standard of 95\% or >100\%.
#'
#' @param data Analytics object to analyze
#' 
#' @return a
#' 
analyze_linkage <- function(data) {
  a <- NULL
  
  data %<>%
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
        + VMMC_CIRC.N.Age_Sex_HIVStatus.T.Positive,
      HTS_TST.Linkage.T = 
        dplyr::case_when(
          HTS_TST_POS.T == 0 ~ NA_real_,
          TRUE ~ 
            (TX_NEW.N.Age_Sex_HIVStatus.T + TX_NEW.N.KeyPop_HIVStatus.T)
          / (HTS_TST_POS.T)
        )
    )
  
  linkage_issues <- data %>%
    dplyr::filter((HTS_TST.Linkage.T < 0.95 | HTS_TST.Linkage.T > 1)
  # Need to analyze <01 linkage separately due to EID
                  & (Age != "<01" | is.na(Age))) %>%
    dplyr::select(PSNU, psnuid, Age, Sex, KeyPop,
                  HTS_TST.Linkage.T, HTS_TST_POS.T, TX_NEW.N.Age_Sex_HIVStatus.T,
                  TX_NEW.N.KeyPop_HIVStatus.T)
  
  if (NROW(linkage_issues) > 0 ) {
    
    a$test_results <- linkage_issues
    attr(a$test_results, "test_name") <- "Linkage rate issues"
    
    national_avg_linkage <- data %>%
      dplyr::filter(Age != "<01" | is.na(Age)) %>%
      dplyr::mutate(
        HTS_TST_POS.KeyPop.T =
          dplyr::if_else(is.na(KeyPop), 0, HTS_TST_POS.T),
        HTS_TST_POS.T = dplyr::if_else(is.na(KeyPop), HTS_TST_POS.T, 0)
      ) %>%
      dplyr::select(
       HTS_TST_POS.T,
       HTS_TST_POS.KeyPop.T,
       TX_NEW.N.Age_Sex_HIVStatus.T,
       TX_NEW.N.KeyPop_HIVStatus.T) %>%
      dplyr::summarise_all(list(sum), na.rm = T) %>%
      dplyr::mutate(
       HTS_TST.Linkage.T = 
         TX_NEW.N.Age_Sex_HIVStatus.T / HTS_TST_POS.T,
       HTS_TST.KeyPop.Linkage.T =
         TX_NEW.N.KeyPop_HIVStatus.T / HTS_TST_POS.KeyPop.T
      )
    
    a$msg <- 
      paste0(
        "WARNING! LINKAGE RATES <95% OR >100%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(linkage_issues$psnuid)), " of ",
            length(unique(data$psnuid)))),
        " PSNUs affected.\n\n\t* ",
        "Lowest linkage rate observed: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(linkage_issues$HTS_TST.Linkage.T))),
        "\n\n\t* ",
        "National average GenPop linkage rate: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(national_avg_linkage$HTS_TST.Linkage.T))),
        "\n\n\t* ",
        "National average KeyPop linkage rate: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(national_avg_linkage$HTS_TST.KeyPop.Linkage.T))),
        "\n")
    
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
  
  # Combine MER and SUBNAT data ####
  data <- d$datim$MER %>%
    dplyr::bind_rows(d$datim$subnat_impatt) %>%
    dplyr::mutate(attributeOptionCombo = "HllvX50cXC0") %>%
    dplyr::group_by(
      dataElement, period, orgUnit, categoryOptionCombo, attributeOptionCombo) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    
  # Adorn metadata
    adorn_import_file() %>%
    dplyr::select(PSNU = dp_psnu, psnuid = psnu_uid,
                  indicator_code, Age, Sex, KeyPop, value)
  
  # Prepare model data ####
  #TODO: Generalize this as function
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
  data %<>%
    dplyr::bind_rows(model_data) %>%
    dplyr::arrange(indicator_code, PSNU, Age, Sex, KeyPop) %>%
    tidyr::pivot_wider(names_from = indicator_code,
                       values_from = value) %>%
    addcols((cop20_data_pack_schema %>%
                dplyr::filter(col_type %in% c("target", "past")) %>%
                dplyr::pull(indicator_code)),
            type = "numeric") %>%
    dplyr::mutate_at(dplyr::vars(-PSNU,-psnuid,-Age,-Sex,-KeyPop),
                     tidyr::replace_na, 0)
  
  # TEST: Retention Rates ####
  a <- analyze_retention(data)
  
  if (!is.null(a)) {
    d$info$analytics_warning_msg <- append(d$info$analytics_warning_msg, a$msg)
    d$tests$retention <- a$test_results
  }
  
  # TEST: Linkage rates ####
  a <- analyze_linkage(data)
  
  if (!is.null(a)) {
    d$info$analytics_warning_msg <- append(d$info$analytics_warning_msg, a$msg)
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
