library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "Mock Data Pack/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")

# For Generating Individual Data Packs ####
generation_list <- c("Central America and Brazil")

pick <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# # For Production run ####
# pick <- datapackr::COP21_datapacks_countries

# Execution ####
for (i in seq_len(pick)) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  packTool(model_data_path = model_data_path,
           tool = "Data Pack",
           datapack_name = pick[[i, 1]],
           country_uids = unlist(pick[[i, 2]]),
           template_path = NULL,
           cop_year = 2021,
           output_folder = output_folder,
           results_archive = FALSE)
}
