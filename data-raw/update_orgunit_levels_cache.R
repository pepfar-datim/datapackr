

loginToDATIM(Sys.getenv("PROD_CREDS"), d2_session_name = "prod")
loginToDATIM(Sys.getenv("TEST_CREDS"), d2_session_name = "cop_test")

fetchOrgunitLevels <- function(cop_year, d2_session) {
  ou_levels <- datimutils::getDataStoreKey("dataSetAssignments", "orgUnitLevels", d2_session = d2_session)
  createOULevelRow <- function(x) {
    data.frame(iso3 = x$iso3,
               iso4 = x$iso4,
               ou = x$name3,
               country_name = x$name4,
               country_level = x$country,
               facility_level = x$facility,
               community_level = x$community,
               prioritization = x$prioritization
               )
  }

  ou_levels <- purrr::map_df(ou_levels, createOULevelRow)
  ou_levels$cop_year <- cop_year

  ou_levels
}

ous <- valid_OrgUnits %>% dplyr::select(country_name, ou_uid, country_uid) %>% dplyr::distinct()

cop21_ou_levels <- dataset_levels %>%  dplyr::filter(cop_year == 2021)
cop22_ou_levels <- dataset_levels %>%  dplyr::filter(cop_year == 2022)
cop23_ou_levels <- fetchOrgunitLevels(2023, prod) %>% dplyr::left_join(ous, by = "country_name")

dataset_levels <- rbind(cop21_ou_levels, cop22_ou_levels, cop23_ou_levels) %>%
  dplyr::mutate(country_name = ifelse(country_name == "", ou, country_name),
                iso4 = ifelse(iso4 == "", iso3, iso4))


usethis::use_data(dataset_levels,  compress = "xz", overwrite = TRUE)
