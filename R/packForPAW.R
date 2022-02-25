#' @export
#' @title Pack For PAW
#'
#' @description
#' Prepare dataset for use in PAW.
#'
#' @inheritParams datapackr_params
#'
#' @return Data frame ready for use in PAW
#'
packForPAW <- function(d) {

  map_des_cocs_local <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year)

  PSNUs <- datapackr::valid_PSNUs %>%
    dplyr::mutate(
      ou_id = purrr::map_chr(ancestors, list("id", 3), .default = NA),
      ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
      snu1_id = dplyr::if_else(
        condition = is.na(purrr::map_chr(ancestors, list("id", 4), .default = NA)),
        true = psnu_uid,
        false = purrr::map_chr(ancestors, list("id", 4), .default = NA)),
      snu1 = dplyr::if_else(
        condition = is.na(purrr::map_chr(ancestors, list("name", 4), .default = NA)),
        true = psnu,
        false = purrr::map_chr(ancestors, list("name", 4), .default = NA))
    ) %>%
    dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id, psnu, psnu_uid)

    d$data$PAW <- d$data$SNUxIM %>%
      dplyr::bind_rows(d$data$SUBNAT_IMPATT) %>%
      dplyr::left_join(PSNUs, by = c("psnuid" = "psnu_uid")) %>%
      dplyr::mutate(
        Age =
          dplyr::case_when(
            indicator_code %in% c("PMTCT_EID.N.Age.T.2mo", "PMTCT_EID.N.Age.T.2to12mo") ~ NA_character_,
            TRUE ~ Age
          )) %>%
      dplyr::left_join(map_des_cocs_local,
                       by = c("indicator_code" = "indicator_code",
                              "Age" = "valid_ages.name",
                              "Sex" = "valid_sexes.name",
                              "KeyPop" = "valid_kps.name")) %>%
      dplyr::mutate(mechanism_code = "12345",
                    mechanism_desc = "default",
                    partner_id = NA_character_,
                    partner_desc = NA_character_,
                    fiscal_year = 2021,
                    funding_agency = NA_character_,
                    categoryoptioncombo_id = categoryoptioncombouid,
                    categoryoptioncombo_name = categoryoptioncombo,
                    result_value = NA_character_) %>%
      dplyr::select(
        ou, ou_id, country_name, country_uid, snu1, snu1_id, psnu, psnuid,
        mechanism_code, mechanism_desc, partner_id, partner_desc, funding_agency,
        fiscal_year,
        dataelement_id = dataelement, dataelement_name = dataelement.y,
        indicator = tech_area, numerator_denominator, support_type, hts_modality,
        categoryoptioncombo_id, categoryoptioncombo_name, age = Age, sex = Sex,
        result_value, target_value = value)

  return(d)
}
