library(magrittr)
library(datapackr)

PAW <- datapackr::map_DataPack_DATIM_DEs_COCs %>%
  dplyr::mutate(
    mechanism_code = "12345",
    mechanism_desc = "Henry Mechanism",
    partner_id = NA_character_,
    partner_desc = NA_character_,
    fiscal_year = 2021,
    funding_agency = NA_character_,
    result_value = NA_character_
  ) %>%
  dplyr::select(
    #ou, ou_id, country_name, country_uid, snu1, snu1_id, psnu, psnuid,
    mechanism_code, mechanism_desc, partner_id, partner_desc, funding_agency,
    fiscal_year,
    dataelement_id = dataelement, dataelement_name = dataelement.y,
    indicator = tech_area, numerator_denominator, support_type, hts_modality,
    categoryoptioncombo_id = categoryoptioncombouid,
    categoryoptioncombo_name = categoryoptioncombo,
    age = valid_ages.name, sex = valid_sexes.name, key_population = valid_kps.name,
    result_value
  ) %>%
  dplyr::bind_rows(
    list(
      ((.) %>% dplyr::mutate(mechanism_code = "67890", mechanism_desc = "Scott Mechanism")),
      ((.) %>% dplyr::mutate(mechanism_code = "13579", mechanism_desc = "Ebonnie Mechanism")),
      ((.) %>% dplyr::mutate(mechanism_code = "24680", mechanism_desc = "Pooja Mechanism"))
    )) %>%
  dplyr::mutate(
    target_value = sample(1000, size = NROW(.), replace = TRUE)
  )


PSNUs <- datapackr::valid_PSNUs %>%
  dplyr::filter(country_name %in% c("Malawi","Eswatini","Lesotho")) %>%
  dplyr::mutate(
    ou_id = purrr::map_chr(ancestors, list("id", 3), .default = NA),
    ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
    snu1_id = dplyr::if_else(
      condition = is.na(purrr::map_chr(ancestors, list("id",4), .default = NA)),
      true = psnu_uid,
      false = purrr::map_chr(ancestors, list("id",4), .default = NA)),
    snu1 = dplyr::if_else(
      condition = is.na(purrr::map_chr(ancestors, list("name",4), .default = NA)),
      true = psnu,
      false = purrr::map_chr(ancestors, list("name",4), .default = NA)),
    prioritization = sample(prioritizations$Prioritization, size = NROW(.), replace = TRUE)
  ) %>%
  dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id,
                psnu, psnu_uid, prioritization)


PAW %<>%
  tidyr::crossing(PSNUs) %>%
  dplyr::select(
    ou, ou_id, country_name, country_uid, snu1, snu1_id, psnu, psnuid = psnu_uid,
    prioritization,
    mechanism_code, mechanism_desc, partner_id, partner_desc, funding_agency,
    fiscal_year,
    dataelement_id, dataelement_name,
    indicator, numerator_denominator, support_type, hts_modality,
    categoryoptioncombo_id,
    categoryoptioncombo_name,
    age, sex,
    result_value, target_value
  )

readr::write_csv(
  x = PAW,
  path = "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/DataPackTEST_FULL.csv")