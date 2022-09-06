library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "COP22 OPUs/")

# For Generating Individual Data Packs ####
generation_list <- c("Eswatini")

pick <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# # For Production run ####
# pick <- datapackr::COP21_datapacks_countries

# Execution ####
for (i in seq_len(NROW(pick))) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  packTool(tool = "OPU Data Pack",
           datapack_name = pick$datapack_name[i],
           country_uids = unlist(pick$country_uids[i]),
           template_path = NULL,
           cop_year = 2022,
           output_folder = output_folder,
           results_archive = FALSE)
}
