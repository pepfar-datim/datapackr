library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "Beta Packs/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")

# For Generating Individual Data Packs ####
generation_list <- c("Malawi", "Namibia", "Nigeria", "South Africa",
                        "Uganda", "Zambia", "Zimbabwe", "India")

pick <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# # For Production run ####
# pick <- datapackr::COP21_datapacks_countries

# Execution ####
for (i in seq_len(NROW(pick))) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  packTool(model_data_path = model_data_path,
           tool = "Data Pack",
           datapack_name = pick$datapack_name[i],
           country_uids = unlist(pick$country_uids[i]),
           template_path = NULL,
           cop_year = 2022,
           output_folder = output_folder,
           results_archive = FALSE)
}
