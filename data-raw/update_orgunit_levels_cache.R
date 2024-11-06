# Run this script at least before each major deployment of a Data Pack for review

secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
loginToDATIM(secrets)

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

#Retrieve the list of valid orgunits for the correct years
ous <- valid_OrgUnits %>% dplyr::select(country_name, ou_uid, country_uid) %>% dplyr::distinct()
ous_24 <-valid_OrgUnits_2024 %>% dplyr::select(country_name, ou_uid, country_uid) %>% dplyr::distinct()
ous_25 <-valid_OrgUnits_2024 %>% dplyr::select(country_name, ou_uid, country_uid) %>% dplyr::distinct()

#Capture the previous dataset levels by looking at what is currently in the save datasetlevels.rda
cop23_ou_levels <- dataset_levels %>%  dplyr::filter(cop_year == 2023)
cop24_ou_levels <- dataset_levels %>%  dplyr::filter(cop_year == 2024)

#Capture the current dataset levels by fetching from Datim and sorting by the valid orgunits above
cop25_ou_levels <- fetchOrgunitLevels(2025, d2_default_session) %>%
  dplyr::mutate(country_name = ifelse(country_name == "", ou, country_name),
                iso4 = ifelse(iso4 == "", iso3, iso4)) %>%
  dplyr::left_join(ous_25, by = "country_name")

#Combine the previous two dataframes into one for saving
dataset_levels <- rbind(cop23_ou_levels, cop24_ou_levels, cop25_ou_levels)

#Overwrite current dataset_levels.rda for the package
usethis::use_data(dataset_levels,  compress = "xz", overwrite = TRUE)

#Remember to run cmd + shift + B, restart session, and clear environment before testing.
