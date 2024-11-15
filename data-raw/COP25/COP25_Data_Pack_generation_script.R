library(datapackr)
library(dplyr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "coptest.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "Beta Packs/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")

# For Generating Individual Data Packs ####
generation_list <- c("Malawi",
                     "Zambia",
                     "Rwanda",
                     "Mozambique",
                     "Zimbabwe",
                     "Tajikistan",
                     "India",
                     "Thailand",
                     "Vietnam",
                     "South Africa",
                     "Central America and Brazil",
                     "Caribbean Region",
                     "Cameroon",
                     "Cote d'Ivoire",
                     "Ghana",
                     "Benin")

pick <- datapackr::cop25_datapack_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# test valid org units against cached ####
valid_OrgUnits <- getDataPackOrgUnits(use_cache = FALSE)

compare_diffs <- datapackr::valid_OrgUnits_2025 %>%
  dplyr::full_join(valid_OrgUnits, by = "uid") %>%
  dplyr::filter(is.na(name.x) | is.na(name.y))

if (NROW(compare_diffs) > 0) {
  stop("Valid org units are not up to date! Please update valid org units.")
} else {
  rm(valid_OrgUnits, compare_diffs)
}

# # For Production run ####
pick <- datapackr::cop25_datapack_countries %>%
  dplyr::filter(!datapack_name %in% c("Asia Region", "Western Hemisphere Region", "Turkmenistan"))

# Execution ####
for (i in seq_along(pick$datapack_name)) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  d <- packTool(model_data_path = model_data_path,
                tool = "Data Pack",
                datapack_name = pick$datapack_name[i],
                country_uids = unlist(pick$country_uids[i]),
                template_path = NULL,
                cop_year = 2025,
                output_folder = output_folder,
                results_archive = FALSE)
}
