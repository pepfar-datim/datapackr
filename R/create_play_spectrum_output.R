#' @export
#' @title Create fake Spectrum output for testing
#'
#' @description
#' Creates a fake Spectrum Data Pack export for use in testing with Data Packs.
#' Requires login to DATIM.
#'
#' @inheritParams datapackr_params
#'
#' @return Fake Spectrum dataset
#'
create_play_spectrum_output <- function(country_uids,
                                        cop_year,
                                        output_folder = NULL,
                                        d2_session = dynGet("d2_default_session",
                                                            inherits = TRUE)) {

  if (cop_year != 2021) {
    stop("That COP Year currently isn't supported for processing by create_play_spectrum_output.")
  }

  map_des_cocs_local <- datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year)

  # Get PSNU list ####
  PSNUs <- datapackr::valid_OrgUnits %>%
    dplyr::filter(country_uid %in% country_uids,
                  psnu_type != "Military") %>%
    datapackr::add_dp_label(.) %>%
    dplyr::arrange(dp_label) %>%
    dplyr::select(PSNU = dp_label, psnu_uid = uid)

  # Get some real data from DATIM ####
  spectrum_des <- tibble::tribble(
    ~dataset, ~dataelementuid,
    "subnat", "nF19GOjcnoD", #DIAGNOSED_SUBNAT.T_1
    "subnat", "ctGo7s0K63z", #KP_MAT_SUBNAT.T_1
    "subnat", "eJaChfuqUTs", #PMTCT_ART_SUBNAT.D.T_1
    "subnat", "HVBf6Sgi6Jk", #PMTCT_ART_SUBNAT.N.T_1
    "subnat", "RM8gRoxtsNw", #PMTCT_STAT_SUBNAT.D.T_1
    "subnat", "tAE7ZD7p9zu", #PMTCT_STAT_SUBNAT.N.T_1
    "subnat", "xghQXueYJxu", #TX_CURR_SUBNAT.T_1
    "subnat", "zoKiMGRucOY", #VL_SUPPRESSION_SUBNAT.T_1
    "subnat", "SSun4i7nHlV", #VMMC_CIRC_SUBNAT.T_1
    "subnat", "ZayJeEa6pCa", #VMMC_TOTALCIRC_SUBNAT.T_1
    "impatt", "lJtpR5byqps", #HIV_PREV.T_1
    "impatt", "iwSejvD8cXl", #PLHIV.T_1
    "impatt", "KssDaTsGWnS", #POP_EST.T_1
    "impatt", "LADGHYIE9m1", #KP_ESTIMATES.Pos.T
    "impatt", "P3AT3zcyRhU", #KP_ESTIMATES.Prev.T
    "impatt", "qFyJH6fUPQk" #KP_ESTIMATES.Total.T
  )


  data_datim <- suppressWarnings(datapackr::getCOPDataFromDATIM(country_uids,
                                               cop_year = (cop_year - 1),
                                               datastreams = c("subnat_targets", "impatt"),
                                               d2_session = d2_session)) %>%
  # Accommodate DUIT decision to map IMPATT to cop_year+1 ####
    dplyr::mutate(
      period =
        dplyr::case_when(
          dataElement %in%
            (spectrum_des %>%
              dplyr::filter(dataset == "impatt") %>%
               dplyr::pull(dataelementuid))
            ~ paste0(cop_year, "Oct"),
          TRUE ~ period
        )
    ) %>%
    dplyr::left_join(
      map_des_cocs_local,
      by = c("dataElement" = "dataelementuid",
             "categoryOptionCombo" = "categoryoptioncombouid",
             "period" = "period")) %>%
    dplyr::filter(dataElement %in% spectrum_des$dataelementuid)

  if (any(is.na(data_datim$indicator_code))) {
    stop("Problem mapping target data pulled from DATIM to datapack schema")
  }

  play_spectrum_output <- data_datim %>%
    dplyr::left_join(PSNUs, by = c("orgUnit" = "psnu_uid")) %>%
    dplyr::mutate(area_id = NA_character_,
                  calendar_quarter =
                    dplyr::if_else(indicator_code == "TX_CURR_SUBNAT.R",
                                   paste0("CY", cop_year - 1, "Q4"),
                                   paste0("CY", cop_year, "Q3"))) %>%
    dplyr::select(psnu = PSNU,
                  psnu_uid = orgUnit,
                  area_id,
                  indicator_code,
                  dataelementuid = dataElement,
                  age = valid_ages.name,
                  age_uid = valid_ages.id,
                  sex = valid_sexes.name,
                  sex_uid = valid_sexes.id,
                  calendar_quarter,
                  value)

  # Adjust for PMTCT ####
  pmtct_data <- play_spectrum_output %>%
    dplyr::filter(
      stringr::str_detect(indicator_code, "^PMTCT")
  )

  # Get PMTCT ages/sexes
  pmtct_subnat_cos <- map_des_cocs_local %>%
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
    dplyr::select(-dataelementuid) %>%
    tidyr::pivot_wider(
      names_from = indicator_code,
      values_from = value
    ) %>%
    dplyr::select(psnu_uid, age_uid, sex_uid,
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
    dplyr::group_by(psnu, psnu_uid, area_id,
                    indicator_code, dataelementuid, calendar_quarter) %>%
    dplyr::mutate(
      pop_dist = pop / sum(pop, na.rm = T),
      plhiv_dist = plhiv / sum(plhiv, na.rm = T),
      tx_curr_dist = tx_curr / sum(tx_curr, na.rm = T),
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
    dplyr::select(names(play_spectrum_output))

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
    country_name <- datimutils::getOrgUnits(country_uids,
                                            d2_session = d2_session)
    exportPackr(data = play_spectrum_output,
                output_folder = output_folder,
                tool = "Spectrum Example",
                datapack_name = country_name)
  }

  return(play_spectrum_output)
}
