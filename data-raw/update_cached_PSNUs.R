library(magrittr)
library(datapackr)

secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

# NOTE: Full documentation can be found in data.R
# The current list can be viewed by running View(valid_OrgUnits)
cop_year <- 2024

# valid_OrgUnits ----
# Fetch PSNU values
valid_OrgUnits <- getDataPackOrgUnits(use_cache = FALSE)

# Comparing default valid_OrgUnits list to newly modified list
compare_diffs <- datapackr::valid_OrgUnits_2024 %>%
  dplyr::full_join(valid_OrgUnits, by = "uid") %>%
  dplyr::filter(is.na(name.x) | is.na(name.y))

waldo::compare(datapackr::valid_OrgUnits_2024, valid_OrgUnits)

# Overwriting default list with newly created list
usethis::use_data(valid_OrgUnits,
                  compress = "xz", overwrite = TRUE)

## Rebuild the package (Cmd+Shift+B)


## Save metadata in API for easy access by Data Management Team

shareable <- datapackr::valid_OrgUnits_2024 %>%
  dplyr::select(orgUnit = uid)

output_folder <- paste0(rprojroot::find_package_root_file(),
                        "/data-raw/")

filename <- "cop24_metadata_organisationUnits"

filepath <- paste0(output_folder, filename, ".csv")

utils::write.csv(shareable, filepath, row.names = FALSE)

## Rebuild package again. (Cmd+Shift+B)


# cop_datapack_countries ----
# If anything has changed at country level or above, update dataframe of data pack countries/names

cop24_datapack_countries <- getValidOrgUnits(cop_year = cop_year) %>%
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

save(cop24_datapack_countries, file = "./data/cop24_datapack_countries.rda", compress = "xz")
