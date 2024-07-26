library(magrittr)
library(datapackr)

secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

# NOTE: Full documentation can be found in data.R
# The current list can be viewed by running View(valid_OrgUnits)
cop_year <- 2025

# valid_OrgUnits ----
# Fetch PSNU values
valid_OrgUnits_2025 <- getDataPackOrgUnits(use_cache = FALSE)

# Comparing default valid_OrgUnits list to newly modified list
# we compare against the previous year initially
# once we create the file if we need to update replace 1234 in
# valid_OrgUnits_1234 with the existing cop year
compare_diffs <- datapackr::valid_OrgUnits_2024 %>%
  dplyr::full_join(valid_OrgUnits_2025, by = "uid") %>%
  dplyr::filter(is.na(name.x) | is.na(name.y))

# for cop 25 we compare against the previous year or latest
# if updating replace the cop year as done above
waldo::compare(datapackr::valid_OrgUnits_2024, valid_OrgUnits_2025)

# when initially creating the value will be valid_OrgUnit_copyear
# when updating after, Overwriting default list with newly created list
usethis::use_data(valid_OrgUnits_2025,
                  compress = "xz", overwrite = TRUE)

## Rebuild the package (Cmd+Shift+B)


## Save metadata in API for easy access by Data Management Team

shareable <- datapackr::valid_OrgUnits_2025 %>%
  dplyr::select(orgUnit = uid)

output_folder <- paste0(rprojroot::find_package_root_file(),
                        "/data-raw/")

filename <- "cop25_metadata_organisationUnits"

filepath <- paste0(output_folder, filename, ".csv")

utils::write.csv(shareable, filepath, row.names = FALSE)

## Rebuild package again. (Cmd+Shift+B)


# cop_datapack_countries ----
# If anything has changed at country level or above, update dataframe of data pack countries/names

cop25_datapack_countries <- getValidOrgUnits(cop_year = cop_year) %>%
  dplyr::select(ou, ou_uid, country_name, country_uid) %>%
  dplyr::distinct() %>%
  # dplyr::mutate(
  #   country_name = dplyr::case_when(
  #     country_name %in% c("Barbados", "Guyana", "Jamaica", "Suriname",
  #                         "Trinidad and Tobago")
  #     ~ "Caribbean Region",
  #     country_name %in% c("Brazil", "Costa Rica", "El Salvador", "Guatemala",
  #                         "Honduras", "Nicaragua", "Panama")
  #     ~ "Central America and Brazil",
  #     TRUE ~ country_name)) %>%
  # dplyr::filter(
  #   !(ou == "Western Hemisphere Region"
  #     & !country_name %in% c("Caribbean Region", "Central America and Brazil"))) %>%
  dplyr::select(-ou, -ou_uid, datapack_name = country_name) %>%
  dplyr::group_by(datapack_name) %>%
  dplyr::summarise(country_uid = list(country_uid)) %>%
  dplyr::rename(country_uids = country_uid)

save(cop25_datapack_countries, file = "./data/cop25_datapack_countries.rda", compress = "xz")
