# library(datapackr)
# Note you'll probably neeed to use dev

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

#Set environment variables
#This is purposefully not saved in the package for security reasons
Sys.setenv(DATAPACK_MODEL_PATH = "~/Desktop/DP_1030/model_data_pack_input_24_20230929_1_flat_de3d50e.rds")
Sys.setenv(output_folder = "~/Documents/Repos/datapackr")

# For Generating Individual Data Packs ####
generation_list <- c("Malawi")

pick <- datapackr::cop_datapack_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# test valid org units against cached ####
valid_OrgUnits <- getDataPackOrgUnits(use_cache = FALSE)

compare_diffs <- datapackr::valid_OrgUnits %>%
  dplyr::full_join(valid_OrgUnits, by = "uid") %>%
  dplyr::filter(is.na(name.x) | is.na(name.y))

if (NROW(compare_diffs) > 0) {
  stop("Valid org units are not up to date! Please update valid org units.")
} else {
  rm(valid_OrgUnits, compare_diffs)
}

# # For Production run ####
# pick <- datapackr::cop_datapack_countries

# Execution ####
for (i in seq_along(pick$datapack_name)) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  d <- packTool(model_data_path = "~/Desktop/DP_1030/model_data_pack_input_24_20230929_1_flat_de3d50e.rds",
                tool = "Data Pack",
                datapack_name = pick$datapack_name[i],
                country_uids = unlist(pick$country_uids[i]),
                template_path = NULL,
                cop_year = 2024,
                output_folder = "~/Documents/Repos/datapackr",
                season = "COP",
                results_archive = FALSE)
}
