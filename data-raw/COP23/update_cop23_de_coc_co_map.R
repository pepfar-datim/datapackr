# Point to DATIM login secrets ----
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
# datimutils::loginToDATIM("~/.secrets/datim.json")
datimutils::loginToDATIM(secrets)
cop_year <- 2023

dp_map <- datapackr::update_de_coc_co_map(cop_year = 2023,
                                          d2_session = dynGet("d2_default_session",
                                                              inherits = TRUE))
#dp_map <- update_de_coc_co_map(cop_year, d2_session)

# Compare old and new maps for accuracy ####
new <- dp_map %>%
  dplyr::select(-categoryoption_specified)

compare_diffs <- datapackr::cop23_map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified) %>%
  dplyr::full_join(new, by = c("indicator_code",
                               "dataelementuid",
                               "categoryoptioncombouid",
                               "FY",
                               "valid_ages.name", "valid_ages.id", "valid_sexes.name",
                               "valid_sexes.id", "valid_kps.name", "valid_kps.id",
                               "categoryOptions.ids", "support_type", "resultstatus", "resultstatus_inclusive")) %>%
  dplyr::filter(is.na(indicator_code) | is.na(dataelementname.x) | is.na(dataelementname.y))

waldo::compare(datapackr::cop23_map_DataPack_DATIM_DEs_COCs, dp_map)



cop23_map_DataPack_DATIM_DEs_COCs <- dp_map
save(cop23_map_DataPack_DATIM_DEs_COCs, file = "./data/cop23_map_DataPack_DATIM_DEs_COCs.rda", compress = "xz")
