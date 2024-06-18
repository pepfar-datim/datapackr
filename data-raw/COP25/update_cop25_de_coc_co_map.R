# Point to DATIM login secrets ----
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
# datimutils::loginToDATIM("~/.secrets/cop-test.json")
datimutils::loginToDATIM(secrets)
cop_year <- 2025

# Patch until MER 3.0 deployed
# dp_map <- datapackr::cop23_map_DataPack_DATIM_DEs_COCs %>%
#   dplyr::mutate(
#     FY = FY + 1,
#     period = dplyr::case_when(
#       period == "2022Oct" ~ "2023Oct",
#       period == "2023Oct" ~ "2024Oct",
#       period == "2024Oct" ~ "2025Oct",
#       period == "2025Oct" ~ "2026Oct", #added cop25
#       TRUE ~ period),
#     period_dataset = stringr::str_replace(period_dataset, "FY26", "FY27"), #added cop25
#     period_dataset = stringr::str_replace(period_dataset, "FY25", "FY26"),
#     period_dataset = stringr::str_replace(period_dataset, "FY24", "FY25"),
#     period_dataset = stringr::str_replace(period_dataset, "FY23", "FY24"),
#   )

dp_map <- datapackr::update_de_coc_co_map(cop_year,
                                         d2_session = dynGet("d2_default_session",
                                                             inherits = TRUE))

# Compare old and new maps for accuracy ####
new <- dp_map %>%
  dplyr::select(-categoryoption_specified)

compare_diffs <- datapackr::cop25_map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified) %>%
  dplyr::full_join(new, by = c("indicator_code",
                               "dataelementuid",
                               "categoryoptioncombouid",
                               "FY",
                               "valid_ages.name", "valid_ages.id", "valid_sexes.name",
                               "valid_sexes.id", "valid_kps.name", "valid_kps.id",
                               "categoryOptions.ids", "support_type", "resultstatus", "resultstatus_inclusive")) %>%
  dplyr::filter(is.na(indicator_code) | is.na(dataelementname.x) | is.na(dataelementname.y))

waldo::compare(datapackr::cop25_map_DataPack_DATIM_DEs_COCs,
               dp_map,
               max_diffs = Inf)


cop25_map_DataPack_DATIM_DEs_COCs <- dp_map
usethis::use_data(cop25_map_DataPack_DATIM_DEs_COCs, overwrite = TRUE, compress = "xz")

## Rebuild package again. (Cmd+Shift+B)

## Save metadata in API for easy access by Data Management Team

shareable <- datapackr::cop25_map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(dataElement = dataelementuid,
                period,
                categoryOptionCombo = categoryoptioncombouid)

output_folder <- paste0(rprojroot::find_package_root_file(),
                        "/data-raw/")

filename <- "cop25_metadata_DEsCOCs"

filepath <- paste0(output_folder, filename, ".csv")

utils::write.csv(shareable, filepath, row.names = FALSE)

## Rebuild package again. (Cmd+Shift+B)
