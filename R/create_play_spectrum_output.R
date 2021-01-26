#' @export
#' @importFrom magrittr %>% %<>%
#' @title Create fake Spectrum output for testing
#'
#' @description
#' Creates a fake Spectrum Data Pack export for use in testing with Data Packs.
#' Requires login to DATIM.
#'
#' @param country_uids Unique IDs for countries to include in the Data Pack.
#' For full list of these IDs, see \code{datapackr::dataPackMap}.
#' @param  cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export. If left as \code{NULL}, will not output externally.
#'
#' @return Fake Spectrum dataset
#'
create_play_spectrum_output <- function(country_uids,
                                        cop_year,
                                        output_folder = NULL) {
  
  if (cop_year == 2021) {
    map_DataPack_DATIM_DEs_COCs_local <- datapackr::map_DataPack_DATIM_DEs_COCs
  } else if (cop_year == 2020) {
    map_DataPack_DATIM_DEs_COCs_local <- datapackr::cop20_map_DataPack_DATIM_DEs_COCs
  } else {
    stop("That COP Year currently isn't supported for processing by create_play_spectrum_output.")
  }
  
  # Get PSNU list ####
  PSNUs <- datapackr::valid_PSNUs %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    datapackr::add_dp_psnu(.) %>%
    dplyr::arrange(dp_psnu) %>%
    dplyr::select(PSNU = dp_psnu, psnu_uid) %>%
    dplyr::filter(!stringr::str_detect(PSNU, "\\[#Military\\]"))
  
  # Get some real data from DATIM ####
  spectrum_des <- c(
    "KssDaTsGWnS", #POP_EST
    "iwSejvD8cXl", #PLHIV
    #"nF19GOjcnoD", #DIAGNOSED_SUBNAT
    "lJtpR5byqps", #HIV_PREV
    "xghQXueYJxu", #TX_CURR_SUBNAT
    #"zoKiMGRucOY", #VL_SUPPRESSION_SUBNAT
    "RM8gRoxtsNw", #PMTCT_STAT_SUBNAT D
    "tAE7ZD7p9zu", #PMTCT_STAT_SUBNAT N
    "eJaChfuqUTs", #PMTCT_ART_SUBNAT D
    "HVBf6Sgi6Jk", #PMTCT_ART_SUBNAT N
    #"ctGo7s0K63z", #KP_MAT_SUBNAT
    #"qFyJH6fUPQk", #KP_ESTIMATES (Total Size)
    #"LADGHYIE9m1", #KP_ESTIMATES (Positive)
    #"P3AT3zcyRhU", #KP_ESTIMATES (Prevalence)
    "SSun4i7nHlV", #VMMC_CIRC_SUBNAT
    "ZayJeEa6pCa" #VMMC_TOTALCIRC_SUBNAT
  )
  
  data_datim <- datapackr::getCOPDataFromDATIM(country_uids,
                                               cop_year = 2020) %>%
    dplyr::left_join(
      map_DataPack_DATIM_DEs_COCs_local,
      by = c("data_element_uid" = "dataelement",
             "category_option_combo_uid" = "categoryoptioncombouid")) %>%
  # Map to renovated indicator_codes
    dplyr::left_join(datapackr::updated_indicator_codes,
                     by = c("indicator_code"))
  
  if (any(is.na(data_datim$indicator_code))) {
    stop("Problem mapping target data pulled from DATIM to datapack schema")
  }
  
  indicator_codes <- datapackr::cop20_data_pack_schema %>% 
    dplyr::filter(dataelement_dsd %in% spectrum_des &
                    dataset %in% c("impatt", "subnat")) %>%
    .[["indicator_code"]]
  
  play_spectrum_output <- data_datim %>%
    dplyr::filter(indicator_code %in% indicator_codes) %>%
    dplyr::left_join(PSNUs, by = c("org_unit_uid" = "psnu_uid")) %>%
    dplyr::select(#support_type,
                  #period,
                  psnu = PSNU,
                  psnu_uid = org_unit_uid,
                  indicator_code = indicator_code_updated,
                  #dataelement = dataelement.y,
                  #dataelementuid = data_element_uid,
                  age = valid_ages.name,
                  age_uid = valid_ages.id,
                  sex = valid_sexes.name,
                  sex_uid = valid_sexes.id,
                  #kp_option_uid = valid_kps.id,
                  #KeyPop = valid_kps.name,
                  value = datim_value) %>%
    dplyr::mutate(
      indicator_code = 
        stringr::str_replace(indicator_code, "\\.T$", "\\.T_1")
        )
  
  # Adjust for PMTCT ####
  pmtct_data <- play_spectrum_output %>%
    dplyr::filter(
      stringr::str_detect(indicator_code, "^PMTCT")
  )
  
  # Get PMTCT ages/sexes
  pmtct_subnat_cos <- map_DataPack_DATIM_DEs_COCs_local %>%
    dplyr::filter(indicator_code == "PMTCT_STAT.D.Age_Sex.T") %>%
    dplyr::select(
      age = valid_ages.name,
      age_uid = valid_ages.id,
      sex = valid_sexes.name,
      sex_uid = valid_sexes.id
    ) %>%
    dplyr::distinct()
  
  pmtct_data %<>%
    dplyr::select(-age, -age_uid, -sex, -sex_uid) %>%
    tidyr::crossing(pmtct_subnat_cos)
  
  # Pull in Host Country data to aid in distribution
  host_country_data <- play_spectrum_output %>%
    dplyr::filter(
      stringr::str_detect(indicator_code, "^(POP_EST|PLHIV|TX_CURR_SUBNAT)")
    ) %>%
    tidyr::pivot_wider(
      names_from = indicator_code,
      values_from = value
    ) %>%
    dplyr::select(-psnu, -age, -sex,
                  pop = `POP_EST.T_1`,
                  plhiv = `PLHIV.T_1`,
                  tx_curr = `TX_CURR_SUBNAT.T_1`)
  
  pmtct_data %<>%
    dplyr::left_join(
      host_country_data,
      by = c("psnu_uid", "age_uid", "sex_uid")
    )
  
  # Distribute PMTCT data
  pmtct_data %<>%
    dplyr::group_by(psnu, psnu_uid, indicator_code) %>%
    dplyr::mutate(
      pop_dist = pop/sum(pop, na.rm = T),
      plhiv_dist = plhiv/sum(plhiv, na.rm = T),
      tx_curr_dist = tx_curr/sum(tx_curr, na.rm = T),
      value = dplyr::case_when(
        stringr::str_detect(indicator_code, "PMTCT_STAT_SUBNAT\\.D")
          ~ round_trunc(value * pop_dist),
        stringr::str_detect(indicator_code, "PMTCT_STAT_SUBNAT\\.N|PMTCT_ART_SUBNAT\\.D")
          ~ round_trunc(value * plhiv_dist),
        stringr::str_detect(indicator_code, "PMTCT_ART_SUBNAT\\.N")
          ~ round_trunc(value * tx_curr_dist)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-pop, -plhiv, -tx_curr, -pop_dist, -plhiv_dist, -tx_curr_dist)
  
  # Add PMTCT data back to other data
  play_spectrum_output %<>%
    dplyr::filter(
      !stringr::str_detect(indicator_code, "PMTCT_(STAT|ART)_SUBNAT\\.(D|N)")
    ) %>%
    dplyr::bind_rows(pmtct_data)
  
  # Add randomized RSEs ####
  play_spectrum_output %<>%
    dplyr::mutate(
      age_sex_rse = sample(1:60, NROW(.), replace = T),
      district_rse = sample(1:50, NROW(.), replace = T)
    )

  # Export
  if (!is.null(output_folder)) {
    print("Saving...")
    country_name <- datimutils::getOrgUnits(country_uids)
    exportPackr(data = play_spectrum_output,
                output_path = output_folder,
                type = "Spectrum Example",
                datapack_name = country_name)
  }
  
  return(play_spectrum_output)  
}
