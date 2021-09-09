

htsModalities<-function() {
c("HTS_INDEX_COM.New.Pos.T",
          "HTS_INDEX_FAC.New.Pos.T",
          "HTS_TST.EW.Pos.T",
          "HTS_TST.Inpat.Pos.T",
          "HTS_TST.Maln.Pos.T",
          "HTS_TST.MobileCom.Pos.T",
          "HTS_TST.OtherCom.Pos.T",
          "HTS_TST.Other.Pos.T",
          "HTS_TST.Peds.Pos.T",
          "HTS_TST.PostANC1.Pos.T",
          "HTS_TST.STI.Pos.T",
          "HTS_TST.VCT.Pos.T",
          "PMTCT_STAT.N.New.Pos.T",
          "TB_STAT.N.New.Pos.T",
          "VMMC_CIRC.Pos.T")
}
#' @export
#' @title Check Data Pack for <90\% PMTCT_EID from ≤02 months
#'
#' @description Check data gathered from Data Pack to identify cases where
#' the targeted percent of infants (<1 yr) born to HIV-positive women tested for 
#' HIV (EID) between 0 and 2 months old is less than 90\%.
#'
#' @param data Analytics object to analyze
#'
#' @return a
#'
analyze_eid_2mo <- function(data) {
  a <- NULL
  
  analysis <- data %>%
    dplyr::mutate(
      PMTCT_EID.T = PMTCT_EID.N.12.T + PMTCT_EID.N.2.T,
      PMTCT_EID.2mo.rate = PMTCT_EID.N.2.T / PMTCT_EID.T
    ) %>%
    dplyr::filter(!is.na(PMTCT_EID.2mo.rate)) %>%
    dplyr::select(
      psnu, psnu_uid, age, sex, key_population,
      PMTCT_EID.T,
      PMTCT_EID.N.2.T,
      PMTCT_EID.N.12.T,
      PMTCT_EID.2mo.rate)
  
  issues <- analysis %>%
    dplyr::filter(PMTCT_EID.2mo.rate < 0.9)
  
  if (NROW(issues) > 0 ) {
    
    a$test_results <- issues
    attr(a$test_results, "test_name") <- "PMTCT_EID coverage by 2 months issues"
    
    national_avg <- analysis %>%
      dplyr::select(
        PMTCT_EID.T,
        PMTCT_EID.N.2.T) %>%
      dplyr::summarise(
        PMTCT_EID.T =
          sum(PMTCT_EID.T, na.rm = T),
        PMTCT_EID.N.2.T =
          sum(PMTCT_EID.N.2.T, na.rm = T)) %>%
      dplyr::mutate(
        PMTCT_EID.2mo.rate =
          PMTCT_EID.N.2.T / PMTCT_EID.T)
    
    a$msg <- 
      paste0(
        "WARNING! PMTCT_EID coverage by 2 months old < 90%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(issues$psnu_uid)), " of ",
            length(unique(analysis$psnu_uid)))),
        " PSNUs affected.\n\n\t* ",
        "Total rate of EID coverage by 2 months across all PSNUs: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * national_avg$PMTCT_EID.2mo.rate)),
        "\n\n\t* ",
        "Lowest observed rate of EID coverage by 2 months: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * min(issues$PMTCT_EID.2mo.rate))),
        "\n")
    
  }
  
  return(a)
}


#' @export
#' @title Check Data Pack data for VMMC_CIRC Indeterminate Rate > 5\%
#'
#' @description Check data gathered from Data Pack to identify cases where
#' the expected number of VMMC_CIRC Indeterminate patients is greater than
#' 5\% of the total number of VMMC_CIRC patients.
#'
#' @param data Analytics object to analyze
#'
#' @return a
#'
analyze_vmmc_indeterminate <- function(data) {
  a <- NULL

  issues <- data %>%
    dplyr::mutate(
      VMMC_CIRC.T = 
        (VMMC_CIRC.Pos.T
         + VMMC_CIRC.Neg.T
         + VMMC_CIRC.Unk.T),
      VMMC_CIRC.indeterminateRate =
        (VMMC_CIRC.Unk.T) /
        (VMMC_CIRC.T)) %>%
    dplyr::filter(VMMC_CIRC.indeterminateRate > 0.05, is.na(key_population)) %>%
    dplyr::select(
      psnu, psnu_uid, age, sex, key_population,
      VMMC_CIRC.T,
      VMMC_CIRC.Pos.T,
      VMMC_CIRC.Neg.T,
      VMMC_CIRC.Unk.T,
      VMMC_CIRC.indeterminateRate)

  if (NROW(issues) > 0 ) {
  
    a$test_results <- issues
    attr(a$test_results, "test_name") <- "VMMC Indeterminate rate issues"

    national_avg <- data %>%
      dplyr::select(
        VMMC_CIRC.Pos.T,
        VMMC_CIRC.Neg.T,
        VMMC_CIRC.Unk.T) %>%
      dplyr::summarise(
        VMMC_CIRC.Pos.T =
          sum(VMMC_CIRC.Pos.T, na.rm = T),
        VMMC_CIRC.Neg.T =
          sum(VMMC_CIRC.Neg.T, na.rm = T),
        VMMC_CIRC.Unk.T =
          sum(VMMC_CIRC.Unk.T, na.rm = T)) %>%
      dplyr::mutate(
        VMMC_CIRC.indeterminateRate =
          (VMMC_CIRC.Unk.T) /
          (VMMC_CIRC.Pos.T
           + VMMC_CIRC.Neg.T
           + VMMC_CIRC.Unk.T)
      )

    a$msg <- 
      paste0(
        "WARNING! VMMC_CIRC Indeterminate > 5% : \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(issues$psnu_uid)), " of ",
            length(unique(data$psnu_uid)))),
        " PSNUs affected.\n\n\t* ",
        "Highest indeterminate/not tested rate observed: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * max(
                               issues$VMMC_CIRC.indeterminateRate
                               ))),
        "\n\n\t* ",
        "National average indeterminate/not tested rate: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * max(
                               national_avg$VMMC_CIRC.indeterminateRate
                             ))),
        "\n")
  
  }

  return(a)
}


#' @export
#' @title Check Data Pack data for PMTCT Known Pos Ratio > 75\%.
#'
#' @description Check data gathered from Data Pack to identify cases where
#' PMTCT Known Pos Ratio > 75\%.
#'
#' @param data Analytics object to analyze
#'
#' @return a
#'
analyze_pmtctknownpos <- function(data) {
  a <- NULL

  issues <- data %>%
    dplyr::filter(is.na(key_population)) %>%
    dplyr::mutate(
      PMTCT_STAT.N.Total =
        PMTCT_STAT.N.New.Pos.T
        + PMTCT_STAT.N.KnownPos.T
        + PMTCT_STAT.N.New.Neg.T,
      knownpos_ratio =
        (PMTCT_STAT.N.KnownPos.T / PMTCT_STAT.N.Total)) %>%
    dplyr::select(
      psnu, psnu_uid, age, sex, key_population,
      PMTCT_STAT.N.Total,
      PMTCT_STAT.N.New.Pos.T,
      PMTCT_STAT.N.KnownPos.T,
      PMTCT_STAT.N.New.Neg.T,
      knownpos_ratio
    ) %>%
    dplyr::filter(!is.na(knownpos_ratio)) %>% 
    dplyr::filter(
      knownpos_ratio > 0.75
    )

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "PMTCT Known Pos issues"

    a$msg <-
      paste0(
        "WARNING! PMTCT KNOWN POS Ratio > 75%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(issues$psnu_uid)), " of ",
            length(unique(data$psnu_uid)))),
        " PSNUs affected.\n\n\t* ",
        "Highest Known Pos ratio observed: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * max(issues$knownpos_ratio))),
        "\n")
  }

  return(a)
}


#' @export
#' @title Check Data Pack data for TB Known Pos ratio > 75\%.
#'
#' @description Check data gathered from Data Pack to identify cases where
#' TB Known Pos ratio > 75\%
#'
#' @param data Analytics object to analyze
#'
#' @return a
#'
analyze_tbknownpos <- function(data) {
  a <- NULL

  issues <- data %>%
    dplyr::mutate(
      TB_STAT.N.Total =
        TB_STAT.N.New.Pos.T
      + TB_STAT.N.KnownPos.T
      + TB_STAT.N.New.Neg.T,
      knownpos_ratio = TB_STAT.N.KnownPos.T / TB_STAT.N.Total) %>%
    dplyr::select(
      psnu, psnu_uid, age, sex, key_population,
      TB_STAT.N.Total,
      TB_STAT.N.New.Pos.T,
      TB_STAT.N.KnownPos.T,
      TB_STAT.N.New.Neg.T,
      knownpos_ratio) %>%
    dplyr::filter(!is.na(knownpos_ratio)) %>% 
    dplyr::filter(
      knownpos_ratio > 0.75)

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "TB Known Pos issues"

    a$msg <-
      paste0(
        "WARNING! TB KNOWN POS Ratio > 75%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(issues$psnu_uid)), " of ",
            length(unique(data$psnu_uid)))),
        " PSNUs affected.\n\n\t* ",
        "Highest Known Pos ratio observed: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * max(issues$knownpos_ratio))),
        "\n")
  }

  return(a)
}


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

  analysis <- data %>%
    dplyr::mutate(
      TX.Retention.T =
        (TX_CURR.T)
      / (TX_CURR.T_1 + TX_NEW.T)
    ) %>% 
    dplyr::filter(!is.na(TX.Retention.T))

  issues <- analysis %>%
    dplyr::filter(TX.Retention.T < 0.98 | TX.Retention.T > 1) %>%
    dplyr::select(
      psnu, psnu_uid, age, sex, key_population,
      TX.Retention.T,
      TX_CURR.T,
      TX_CURR.T_1,
      TX_NEW.T)

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "Retention rate issues"

    national_avg <- data %>%
      dplyr::select(
        TX_CURR.T,
        TX_CURR.T_1,
        TX_NEW.T) %>%
      dplyr::summarise(
        TX_CURR.T = sum(TX_CURR.T, na.rm = T),
        TX_CURR.T_1  = sum(TX_CURR.T_1, na.rm = T),
        TX_NEW.T = sum(TX_NEW.T, na.rm = T)) %>%
      dplyr::mutate(
        TX.Retention.T =
          (TX_CURR.T)
        / (TX_CURR.T_1 + TX_NEW.T)
      )

    a$msg <-
      paste0(
        "WARNING! RETENTION RATES <98% OR >100%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(issues$psnu_uid)), " of ",
            length(unique(data$psnu_uid)))),
        " PSNUs affected.\n\n\t* ",
        "Lowest retention rate observed: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * min(issues$TX.Retention.T))),
        "\n\n\t* ",
        "National average retention rate: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * min(national_avg$TX.Retention.T))),
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

  analysis <- data %>%
    dplyr::mutate(
      HTS_TST_POS.T =rowSums(dplyr::select(.,tidyselect::any_of(htsModalities()))),
      HTS_TST.Linkage.T =
        dplyr::case_when(
          HTS_TST_POS.T == 0 ~ NA_real_,
          TRUE ~
            TX_NEW.T
          / HTS_TST_POS.T
        ),
      HTS_TST.KP.Linkage.T = 
        dplyr::case_when(
          HTS_TST.KP.Pos.T == 0 ~ NA_real_,
          TRUE ~
            TX_NEW.KP.T
          / HTS_TST.KP.Pos.T
        )
    )

  issues <- analysis %>%
    dplyr::filter((HTS_TST.Linkage.T < 0.95 | HTS_TST.Linkage.T > 1
                   | HTS_TST.KP.Linkage.T < 0.95 | HTS_TST.KP.Linkage.T > 1)
  # Need to analyze <01 linkage separately due to EID
                   & (age != "<01" | is.na(age))) %>%
    dplyr::select(psnu, psnu_uid, age, sex, key_population,
                  HTS_TST.Linkage.T, HTS_TST_POS.T, TX_NEW.T,
                  HTS_TST.KP.Linkage.T, HTS_TST.KP.Pos.T, TX_NEW.KP.T)

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "Linkage rate issues"

    national_avg <- analysis %>%
      dplyr::filter(age != "<01" | is.na(age)) %>%
      dplyr::select(
       HTS_TST_POS.T,
       TX_NEW.T,
       HTS_TST.KP.Pos.T,
       TX_NEW.KP.T) %>%
      dplyr::summarise_all(list(sum), na.rm = T) %>%
      dplyr::mutate(
       HTS_TST.Linkage.T =
         TX_NEW.T / HTS_TST_POS.T,
       HTS_TST.KP.Linkage.T =
         TX_NEW.KP.T / HTS_TST.KP.Pos.T
      ) 

    a$msg <-
      paste0(
        "WARNING! LINKAGE RATES <95% OR >100%: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(issues$psnu_uid)), " of ",
            length(unique(data$psnu_uid)))),
        " PSNUs affected.\n\n\t* ",
        "Lowest linkage rate observed: ",
        crayon::bold(sprintf("%.1f%%",
                             100 * min(issues$HTS_TST.Linkage.T))),
        "\n\n\t* ",
        "National average Total Population linkage rate: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(national_avg$HTS_TST.Linkage.T))),
        "\n\n\t* ",
        "National average Key Population linkage rate: ",
        crayon::bold(sprintf("%.1f%%", 100 * min(national_avg$HTS_TST.KP.Linkage.T))),
        "\n")

  }

  return(a)

}

#' @export
#' @title Check Data Pack data for low representation of HTS_INDEX_POS
#'
#' @description Check data gathered from Data Pack to identify cases where
#' the proportion of HTS_TST_POS represented by HTS_INDEX_POS is too low for the
#' given ART Coverage rate:
#'
#' \tabular{cc}{
#'   \strong{ART Coverage}\tab  \strong{HTS_INDEX_POS \% of HTS_TST_POS}\cr
#'   <70\%\tab  30\%\cr
#'   70\% <= x < 80\%\tab  50\%\cr
#'   >= 80\%\tab  75\%\cr
#' }
#'
#' @param data Analytics object to analyze
#'
#' @return a
#'
analyze_indexpos_ratio <- function(data) {
  a <- NULL

  analysis <- data %>%
    dplyr::filter(is.na(key_population)) %>%
    dplyr::select(-age, -sex, -key_population) %>%
    dplyr::group_by(psnu, psnu_uid) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      HTS_TST_POS.T = rowSums(dplyr::select(.,tidyselect::any_of(htsModalities()))),
      HTS_INDEX.total =
        HTS_INDEX_COM.New.Pos.T
        + HTS_INDEX_FAC.New.Pos.T,
      HTS_TST_POS.index_rate =
        dplyr::case_when(
          HTS_TST_POS.T == 0 ~ NA_real_,
          TRUE ~ HTS_INDEX.total
                / (HTS_TST_POS.T)
        ),
      ART_coverage = dplyr::case_when(
          PLHIV.T_1 == 0 ~ NA_real_,
          TRUE ~ TX_CURR_SUBNAT.T_1
                / PLHIV.T_1)) %>%
    dplyr::select(psnu, psnu_uid, TX_CURR_SUBNAT.T_1, PLHIV.T_1, ART_coverage,
                  HTS_INDEX.total, HTS_TST_POS.T, HTS_TST_POS.index_rate)

  issues <- analysis %>%
    dplyr::mutate(
      index_issues =
        (ART_coverage < 0.7 & HTS_TST_POS.index_rate < 0.3)
        | (ART_coverage >= 0.7 & ART_coverage < 0.8 & HTS_TST_POS.index_rate < 0.5)
        | (ART_coverage >= 0.8 & HTS_TST_POS.index_rate < 0.75)) %>%
    dplyr::filter(index_issues) %>%
    dplyr::mutate(
      category = dplyr::case_when(
        (ART_coverage > 1) | (ART_coverage == 0) | is.na(ART_coverage) ~ "Inspect ART Coverage",
        HTS_TST_POS.T < 10 ~ "Low baseline HTS_TST_POS",
        TRUE ~ "Possible under-utilization of Index testing"
      )
    )

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "HTS_INDEX_POS Rate Issues"

    a$msg <-
      paste0(
        "WARNING! HTS_INDEX_POS RATES TOO LOW: \n\n\t* ",
        crayon::bold(
          paste0(
            length(unique(issues$psnu_uid)), " of ",
            length(unique(data$psnu_uid)))),
          " PSNUs affected. \n\n\t* ",
        "Likely root causes: \n\n\t\t- ",
        crayon::bold(length(issues$category[issues$category == "Inspect ART Coverage"])),
          " cases possibly due to faulty ART Coverage statistics",
        "\n\n\t\t- ",
        crayon::bold(length(issues$category[issues$category == "Low baseline HTS_TST_POS"])),
          " cases possibly due to low baseline HTS_TST_POS",
        "\n\n\t\t- ",
        crayon::bold(length(issues$category[issues$category == "Possible under-utilization of Index testing"])),
          " cases possibly due to actual HTS_INDEX_POS rate issue",
        "\n")
  }

  return(a)

}


#' @export
#' @title Check Data Pack data for analytics concerns
#'
#' @description Check data gathered from Data Pack to identify
#' validation concerns at the PSNU level.
#'
#' @param d datapackr object
#' @param model_data_path Filepath to model data produced from most recent DATIM
#' sync.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#'
#' @return d
#'
checkAnalytics <- function(d,
                           model_data_path,
                           d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {

  # Start running log of all warning and information messages ####
  d$keychain$model_data_path <- model_data_path
  d$info$analytics_warning_msg <- NULL
  d$info$has_analytics_error <- FALSE

  # Prepare analytics data ####
  data <- d$data$analytics %>%
    dplyr::select(psnu, psnu_uid,
                  indicator_code, age, sex, key_population, value = target_value) %>%
    dplyr::group_by(dplyr::across(-value)) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()
  
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

    d$info$analytics_warning_msg <- append(d$info$analytics_warning_msg,
                                           analytics_warning_msg)
  }
  
  category_options <- datimutils::getMetadata(end_point = "categoryOptions",
                                              "categories.id:ne:SH885jaRe0o",
                                              d2_session = d2_session)

  model_data_country <- model_data[d$info$country_uids] %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na(value) %>%
    dplyr::left_join(
      datapackr::valid_PSNUs %>%
        dplyr::filter(country_uid %in% d$info$country_uids) %>%
        dplyr::select(psnu, psnu_uid),
      by = c("psnu_uid" = "psnu_uid")
    ) %>%
    dplyr::left_join(dplyr::rename(category_options, age = name),
                     by = c("age_option_uid" = "id")) %>%
    dplyr::left_join(dplyr::rename(category_options, sex = name),
                     by = c("sex_option_uid" = "id")) %>%
    dplyr::left_join(dplyr::rename(category_options, key_population = name),
                     by = c("kp_option_uid" = "id")) %>%
    dplyr::select(names(data))
  
  # Add model_data to analytics dataset ####
  data %<>%
    dplyr::bind_rows(model_data_country) %>%
    dplyr::arrange(dplyr::across(-value)) %>%
    tidyr::pivot_wider(names_from = indicator_code,
                       values_from = value) %>%
    addcols((d$info$schema %>%
                dplyr::filter(col_type %in% c("target", "past"),
                              sheet_name != "PSNUxIM") %>%
                dplyr::pull(indicator_code)),
            type = "numeric") %>%
    dplyr::mutate(dplyr::across(c(-psnu, -psnu_uid, -age, -sex, -key_population),
                     ~tidyr::replace_na(.x, 0)))



  #Apply the list of analytics checks functions  
  funs <- list(
    retention = analyze_retention,
    linkage = analyze_linkage,
    index_rate = analyze_indexpos_ratio,
    pmtctknownpos_issues = analyze_pmtctknownpos,
    tbknownpos_issues = analyze_tbknownpos,
    vmmc_indeterminate_rate = analyze_vmmc_indeterminate,
    eid_coverage_2mo  = analyze_eid_2mo
  )
  
  analytics_checks <-  purrr::map(funs,purrr::exec,data)
  
  d$info$analytics_warning_msg <-
    append(
      d$info$analytics_warning_msg,
      purrr::map(analytics_checks, 
                 function(x)purrr::pluck(x,"msg"))) %>%
    purrr::discard(is.null)
    

  d$tests <-
    append(d$tests,
           purrr::map(analytics_checks, 
                      function(x) purrr::pluck(x,"test_results"))) %>% 
    purrr::discard(is.null)

  # If warnings, show all grouped by sheet and issue ####
  if (!is.null(d$info$analytics_warning_msg) & interactive()) {
    options(warning.length = 8170)

    messages <-
      paste(
        paste(
          seq_along(d$info$analytics_warning_msg),
          ": ", d$info$analytics_warning_msg
          #stringr::str_squish(gsub("\n", "", d$info$analytics_warning_msg))
        ),
        sep = "",
        collapse = "\r\n")

    key <- paste0(
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
