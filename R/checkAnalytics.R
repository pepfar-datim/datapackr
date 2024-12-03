


HTS_POS_Modalities <- function(cop_year) {

    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::select(indicator_code, hts_modality, resultstatus) %>%
    tidyr::drop_na() %>%
    dplyr::filter(resultstatus %in% c("Newly Tested Positives", "Positive")) %>%
    dplyr::distinct() %>%
    dplyr::pull(indicator_code)
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
analyze_eid_2mo <- function(data) { #Doesnt exist for 2025 NOTHING EID RELATED

  a <- NULL

  this_cop_year <- data$cop_year[[1]]

    required_names <- c("PMTCT_EID.D.T",
                        "PMTCT_EID.N.2.T")

  if (!all((required_names %in% names(data)))) {
    a$test_results <- data.frame(msg = "Missing data.")
    attr(a$test_results, "test_name") <- "PMTCT_EID coverage by 2 months issues"
    a$msg <- "Could not analyze PMTCT EID due to missing data."
    return(a)
  }

    analysis <- data %>%
      dplyr::mutate(
        PMTCT_EID.2mo.rate = PMTCT_EID.N.2.T / PMTCT_EID.D.T
      ) %>%
      dplyr::filter(!is.na(PMTCT_EID.2mo.rate)) %>%
      dplyr::select(
        psnu, psnu_uid, age, sex, key_population,
        PMTCT_EID.N.2.T,
        PMTCT_EID.D.T,
        PMTCT_EID.2mo.rate)

  issues <- analysis %>%
    dplyr::filter(round(PMTCT_EID.2mo.rate, 2) < 0.90)

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "PMTCT_EID coverage by 2 months issues"

      national_avg <- analysis %>%
        dplyr::select(
          PMTCT_EID.D.T,
          PMTCT_EID.N.2.T) %>%
        dplyr::summarise(
          PMTCT_EID.D.T =
            sum(PMTCT_EID.D.T, na.rm = TRUE),
          PMTCT_EID.N.2.T =
            sum(PMTCT_EID.N.2.T, na.rm = TRUE)) %>%
        dplyr::mutate(
          PMTCT_EID.2mo.rate =
            PMTCT_EID.N.2.T / PMTCT_EID.D.T)

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
  # This one vmc circ target, total roll up not disaggs below WILL BE ONEVMMC_CIRC.T
  # VMMC SUBNAT HOWEVEER is staying the same, NOT in this block btw

  a <- NULL
  required_names <- c("VMMC_CIRC.Pos.T",
                      "VMMC_CIRC.Neg.T",
                      "VMMC_CIRC.Unk.T")

  if (!all(required_names %in% names(data))) {
    a$test_results <- data.frame(msg = "Missing data.")
    attr(a$test_results, "test_name") <- "VMMC Indeterminate rate issues"
    a$msg <- "Could not analyze VMMC_CIRC Indeterminate Rate due to missing data."
    return(a)
  }

  issues <- data %>%
    dplyr::mutate(
      VMMC_CIRC.T =
        (VMMC_CIRC.Pos.T
         + VMMC_CIRC.Neg.T
         + VMMC_CIRC.Unk.T),
      VMMC_CIRC.indeterminateRate =
        (VMMC_CIRC.Unk.T) / #Unkknow doesnt exits in 25, anything tied disags.
        (VMMC_CIRC.T)) %>%
    dplyr::filter(round(VMMC_CIRC.indeterminateRate, 2) > 0.05, is.na(key_population)) %>%
    dplyr::select(
      psnu, psnu_uid, age, sex, key_population,
      VMMC_CIRC.T,
      VMMC_CIRC.Pos.T,
      VMMC_CIRC.Neg.T,
      VMMC_CIRC.Unk.T,
      VMMC_CIRC.indeterminateRate)

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "VMMC Indeterminate rate issues"

    national_avg <- data %>%
      dplyr::select(
        VMMC_CIRC.Pos.T,
        VMMC_CIRC.Neg.T,
        VMMC_CIRC.Unk.T) %>%
      dplyr::summarise(
        VMMC_CIRC.Pos.T =
          sum(VMMC_CIRC.Pos.T, na.rm = TRUE),
        VMMC_CIRC.Neg.T =
          sum(VMMC_CIRC.Neg.T, na.rm = TRUE),
        VMMC_CIRC.Unk.T =
          sum(VMMC_CIRC.Unk.T, na.rm = TRUE)) %>%
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
  #These are gone for 2025, ANYTHING PMTCT will only see _subnat
  a <- NULL

  this_cop_year <- as.character(data$cop_year[1])

  required_names <- switch(this_cop_year,
                           "2023" =  c("PMTCT_STAT.N.New.Pos.T",
                                       "PMTCT_STAT.N.KnownPos.T",
                                       "PMTCT_STAT.N.New.Neg.T"),
                           "2024" =  c("PMTCT_STAT.N.New.Pos.T",
                                       "PMTCT_STAT.N.Known.Pos.T",
                                       "PMTCT_STAT.N.New.Neg.T"),
                           "2025" =  c("PMTCT_STAT.N.New.Pos.T",
                                       "PMTCT_STAT.N.Known.Pos.T",
                                       "PMTCT_STAT.N.New.Neg.T"),#TEMPORARY
                           stop("Unsupported COP Year"))

  if (!all(required_names %in% names(data))) {
    a$test_results <- data.frame(msg = "Missing data.")
    attr(a$test_results, "test_name") <- "PMTCT Known Pos issues"
    a$msg <- "Could not analyze PMTCT Known Pos issues due to missing data."
    return(a)
  }

  issues <- if (this_cop_year == "2023") {
    data %>%
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
      round(knownpos_ratio, 2) > 0.75
    )
  } else if (this_cop_year >= "2024") { #TEMPORARY should be ==
    data %>%
      dplyr::filter(is.na(key_population)) %>%
      dplyr::mutate(
        PMTCT_STAT.N.Total =
          PMTCT_STAT.N.New.Pos.T
        + PMTCT_STAT.N.Known.Pos.T
        + PMTCT_STAT.N.New.Neg.T,
        knownpos_ratio =
          (PMTCT_STAT.N.Known.Pos.T / PMTCT_STAT.N.Total)) %>%
      dplyr::select(
        psnu, psnu_uid, age, sex, key_population,
        PMTCT_STAT.N.Total,
        PMTCT_STAT.N.New.Pos.T,
        PMTCT_STAT.N.Known.Pos.T,
        PMTCT_STAT.N.New.Neg.T,
        knownpos_ratio
      ) %>%
      dplyr::filter(!is.na(knownpos_ratio)) %>%
      dplyr::filter(
        round(knownpos_ratio, 2) > 0.75
      )
    }

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
  #GONE for 2025
  a <- NULL

  this_cop_year <- as.character(data$cop_year[1])

  required_names <- switch(this_cop_year,
                           "2023" =  c("TB_STAT.N.New.Pos.T",
                                       "TB_STAT.N.KnownPos.T",
                                       "TB_STAT.N.New.Neg.T"),
                           "2024" =  c("TB_STAT.N.New.Pos.T",
                                       "TB_STAT.N.Known.Pos.T",
                                       "TB_STAT.N.New.Neg.T"),
                           "2025" =  c("TB_STAT.N.New.Pos.T",#TEMPORARY
                                       "TB_STAT.N.Known.Pos.T",
                                       "TB_STAT.N.New.Neg.T"),
                           stop("Unsupported COP Year"))

  if (!all(required_names %in% names(data))) {
    a$test_results <- data.frame(msg = "Missing data.")
    attr(a$test_results, "test_name") <- "TB Known Pos issues"
    a$msg <- "Could not analyze TB Known Pos issues due to missing data."
    return(a)
  }

  issues <- if (this_cop_year == "2023") {
    data %>%
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
      round(knownpos_ratio, 2) > 0.75)
    } else if (this_cop_year >= "2024") { #TEMPORARY
      data %>%
      dplyr::mutate(
        TB_STAT.N.Total =
          TB_STAT.N.New.Pos.T
        + TB_STAT.N.Known.Pos.T
        + TB_STAT.N.New.Neg.T,
        knownpos_ratio = TB_STAT.N.Known.Pos.T / TB_STAT.N.Total) %>%
      dplyr::select(
        psnu, psnu_uid, age, sex, key_population,
        TB_STAT.N.Total,
        TB_STAT.N.New.Pos.T,
        TB_STAT.N.Known.Pos.T,
        TB_STAT.N.New.Neg.T,
        knownpos_ratio) %>%
      dplyr::filter(!is.na(knownpos_ratio)) %>%
      dplyr::filter(
        round(knownpos_ratio, 2) > 0.75)
  }

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
  #REMAINING for 2025. Formula will match 2024
  a <- NULL

  this_cop_year <- as.character(data$cop_year[1])

  required_names <- switch(this_cop_year,
                           "2023" =  c("TX_CURR.T",
                                       "TX_CURR.Expected.T_1",
                                       "TX_NEW.T"),
                           "2024" =  c("TX_CURR.T",
                                       "TX_CURR.Expected.T_1",
                                       "TX_NEW.T"),
                           "2025" =  c("TX_CURR.T", #TEMPORARY
                                       "TX_CURR.Expected.T_1",
                                       "TX_NEW.T"),
                           stop("Unsupported COP Year"))


  if (!all(required_names %in% names(data))) {
    a$test_results <- data.frame(msg = "Missing data.")
    attr(a$test_results, "test_name") <- "Retention rate issues"
    a$msg <- "Could not analyze Retention rate issues due to missing data."
    return(a)
  }

    analysis <- data %>%
      dplyr::group_by(psnu, psnu_uid, age, sex, key_population, cop_year) %>%
      dplyr::summarise_all(sum) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        TX.Retention.T =
          (TX_CURR.T)
        / (TX_CURR.Expected.T_1 + TX_NEW.T)
      ) %>%
      dplyr::filter(!is.na(TX.Retention.T)) %>%
      dplyr::mutate(TX.Retention.T = round(TX.Retention.T, 2))

    issues <- analysis %>%
      dplyr::filter(TX.Retention.T < 0.98 | TX.Retention.T > 1) %>%
      dplyr::select(
        psnu, psnu_uid, age, sex, key_population,
        TX.Retention.T,
        TX_CURR.T,
        TX_CURR.Expected.T_1,
        TX_NEW.T)

  if (NROW(issues) > 0) {

    a$test_results <- issues
    attr(a$test_results, "test_name") <- "Retention rate issues"

      national_avg <- data %>%
        dplyr::select(
          TX_CURR.T,
          TX_CURR.Expected.T_1,
          TX_NEW.T) %>%
        dplyr::summarise(
          TX_CURR.T = sum(TX_CURR.T, na.rm = TRUE),
          TX_CURR.Expected.T_1  = sum(TX_CURR.Expected.T_1, na.rm = TRUE),
          TX_NEW.T = sum(TX_NEW.T, na.rm = TRUE)) %>%
        dplyr::mutate(
          TX.Retention.T =
            (TX_CURR.T)
          / (TX_CURR.Expected.T_1 + TX_NEW.T)
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
  #Still exist for 25
  a <- NULL

  #We only have HTS_tst.pos.T for 2025 NO MODALITIES anymore for target setting
  hts_modalities <- HTS_POS_Modalities(data$cop_year[1])

  required_names <- c("TX_NEW.T", "TX_NEW.KP.T")

  if (!all(required_names %in% names(data))) {
    a$test_results <- data.frame(msg = "Missing data.")
    attr(a$test_results, "test_name") <- "Linkage rate issues"
    a$msg <- "Could not analyze Linkage rate issues due to missing data."
    return(a)
  }



  analysis <- data %>%
    dplyr::mutate(age = dplyr::case_when(age %in% c("50-54", "55-59", "60-64", "65+") ~ "50+",
                                         TRUE ~ age)) %>%
    dplyr::mutate(
      HTS_TST_POS.T  = rowSums(dplyr::select(., tidyselect::any_of(hts_modalities))),
      HTS_TST.Linkage.T =
        dplyr::case_when(
          HTS_TST_POS.T == 0 ~ NA_real_,
          TRUE ~
            TX_NEW.T
          / HTS_TST_POS.T #Change HTS_TST.Pos.T for 2025 IF WE WANTED
        ),
      HTS_TST.KP.Linkage.T =
        dplyr::case_when(
          HTS_TST.KP.Pos.T == 0 ~ NA_real_,
          TRUE ~
            TX_NEW.KP.T
          / HTS_TST.KP.Pos.T #CORRECT FOR 25
        )
    )

  issues <- analysis %>%
    dplyr::filter((round(HTS_TST.Linkage.T, 2) < 0.95 | round(HTS_TST.Linkage.T, 2) > 1.0
                   | round(HTS_TST.KP.Linkage.T, 2) < 0.95 | round(HTS_TST.KP.Linkage.T, 2) > 1.0)
  # Need to analyze <01 linkage separately due to EID #2025 NOT TRUE ANYMORE  all captured on same tab
                   & (age != "<01" | is.na(age))) %>% #REMOVE this for 2025
    dplyr::select(psnu, psnu_uid, age, sex, key_population,
                  HTS_TST.Linkage.T, HTS_TST_POS.T, TX_NEW.T,#Change HTS_TST.Pos.T for 25
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
      dplyr::summarise_all(list(sum), na.rm = TRUE) %>%
      dplyr::mutate(
       HTS_TST.Linkage.T =
         TX_NEW.T / HTS_TST_POS.T, #Change HTS_TST.Pos.T for 25
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
analyze_indexpos_ratio <- function(data) { #PROBABLY GONE 2025, Christian will confirm First week of December 2024

  a <- NULL

  this_cop_year <- data$cop_year[[1]]

     required_names <- c("HTS.Index.Pos.T", #This is gone 2025, may make below impossible 613 - 629
                         "PLHIV.T",
                         "TX_CURR_SUBNAT.T")


  if (!all(required_names %in% names(data))) {
    a$test_results <- data.frame(msg = "Missing data.")
    attr(a$test_results, "test_name") <- "HTS_INDEX_POS Rate Issues"
    a$msg <- "Could not analyze HTS_INDEX_POS Rate Issues due to missing data."
    return(a)
  }

  hts_modalities <- HTS_POS_Modalities(this_cop_year) #COP 25 no modalities anymore, so may need to skip entirely

    analysis <- data %>%
      dplyr::filter(is.na(key_population)) %>%
      dplyr::select(-age, -sex, -key_population) %>%
      dplyr::group_by(psnu, psnu_uid) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), sum), .groups = "drop") %>%
      dplyr::mutate(
        HTS_TST_POS.T = rowSums(dplyr::select(., tidyselect::any_of(hts_modalities))),
        HTS_INDEX.total = HTS.Index.Pos.T,
        HTS_TST_POS.index_rate =
          dplyr::case_when(
            HTS_TST_POS.T == 0 ~ NA_real_,
            TRUE ~ HTS_INDEX.total
            / (HTS_TST_POS.T)
          ),
        ART_coverage = dplyr::case_when(
          PLHIV.T == 0 ~ NA_real_,
          TRUE ~ TX_CURR_SUBNAT.T
          / PLHIV.T)) %>%
      dplyr::select(psnu, psnu_uid, TX_CURR_SUBNAT.T, PLHIV.T, ART_coverage,
                    HTS_INDEX.total, HTS_TST_POS.T, HTS_TST_POS.index_rate)

  issues <- analysis %>%
    dplyr::mutate(
      index_issues =
        (round(ART_coverage, 2) < 0.70 & round(HTS_TST_POS.index_rate, 2) < 0.30)
        | (round(ART_coverage, 2) >= 0.70 & round(ART_coverage, 2) < 0.80 & round(HTS_TST_POS.index_rate, 2) < 0.50)
        | (round(ART_coverage, 2) >= 0.80 & round(HTS_TST_POS.index_rate, 2) < 0.75)) %>%
    dplyr::filter(index_issues) %>%
    dplyr::mutate(
      category = dplyr::case_when(
        (ART_coverage > 1.0) | (ART_coverage == 0.0) | is.na(ART_coverage) ~ "Inspect ART Coverage",
        HTS_TST_POS.T < 10.0 ~ "Low baseline HTS_TST_POS",
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

  a

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

  #Exit if there is no analytics
  if (is.null(d$data$analytics)) {
    d$info$analytics_warning_msg <- append(d$info$analytics_warning_msg,
                                           "No analytics data found.")
    return(d)
  }

  # Prepare analytics data ####
  data <- d$data$analytics %>%
    dplyr::select(psnu, psnu_uid,
                  indicator_code, age, sex, key_population, value = target_value) %>%
    dplyr::group_by(dplyr::across(-value)) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  #Special analytics indicators needed for some checksin COP23
  if (d$info$cop_year >= "2023") {
    if (d$info$cop_year < "2025") {pmtct_eid_d <- extractRawColumnData(d, "EID", "PMTCT_EID.D.T")} #GONE 2025 so can remove probs
    tx_curr_expected <- extractRawColumnData(d, "Cascade", c("Age", "Sex", "TX_CURR.Expected.T_1"))
    data <- data %>%
      dplyr::bind_rows(pmtct_eid_d, tx_curr_expected)
  }


  # Prepare model data ####
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
      getValidOrgUnits(d$info$cop_year) %>%
        dplyr::filter(country_uid %in% d$info$country_uids) %>%
        dplyr::select(psnu = name, psnu_uid = uid),
      by = c("psnu_uid" = "psnu_uid") #Watch this for COP25 may have to update to account for TSNU.
    ) %>%
    dplyr::left_join(dplyr::rename(category_options, age = name),
                     by = c("age_option_uid" = "id")) %>%
    dplyr::left_join(dplyr::rename(category_options, sex = name),
                     by = c("sex_option_uid" = "id")) %>%
    dplyr::left_join(dplyr::rename(category_options, key_population = name),
                     by = c("kp_option_uid" = "id")) %>%
    # Special handling for certain category options which
    # have leading zeros in the Datapack
    dplyr::mutate(age = dplyr::case_when(age == "5-9" ~ "05-09",
                                         age == "1-4" ~ "01-04",
                                         age == "1-9" ~ "01-09",
                                         age == "<1" ~ "<01",
                                         TRUE ~ age)) %>%
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
                dplyr::pull(indicator_code) %>%
               unique(.)),
            type = "numeric") %>%
    dplyr::mutate(dplyr::across(c(-psnu, -psnu_uid, -age, -sex, -key_population),
                     ~tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(cop_year = d$info$cop_year)

  #Apply the list of analytics checks functions
  funs <- list(
    retention = analyze_retention,
    linkage = analyze_linkage,
    index_rate = analyze_indexpos_ratio,
    if(copy_year < 2025){pmtctknownpos_issues = analyze_pmtctknownpos},
    tbknownpos_issues = analyze_tbknownpos,
    vmmc_indeterminate_rate = analyze_vmmc_indeterminate,
    if(copy_year < 2025){eid_coverage_2mo  = analyze_eid_2mo}
  )

  analytics_checks <-  purrr::map(funs, purrr::exec, data)


  #Discard any checks which are NULL
  analytics_checks <- purrr::discard(analytics_checks, is.null)

  d$info$analytics_warning_msg <-
    append(
      d$info$analytics_warning_msg,
      purrr::map_chr(analytics_checks, "msg")
    )

  d$tests <-
    append(d$tests,
           purrr::map(analytics_checks, "test_results"))

  # If warnings, show all grouped by sheet and issue ####
  if (!is.null(d$info$analytics_warning_msg) && interactive()) {
    options(warning.length = 8170)

    messages <-
      paste(
        paste(
          seq_len(NROW(d$info$analytics_warning_msg)),
          ": ", d$info$analytics_warning_msg
          # stringr::str_squish(gsub("\n", "", d$info$analytics_warning_msg))
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
