library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
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
                     "Cote d'Ivoire")

pick <- datapackr::cop_datapack_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# # For Production run ####
# pick <- datapackr::cop_datapack_countries

# Execution ####
for (i in seq_along(pick$datapack_name)) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  d <- packTool(model_data_path = model_data_path,
           tool = "Data Pack",
           datapack_name = pick$datapack_name[i],
           country_uids = unlist(pick$country_uids[i]),
           template_path = NULL,
           cop_year = 2023,
           output_folder = output_folder,
           results_archive = FALSE)
}
