library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "COP21 OPUs/")

# For Generating Individual Data Packs ####
generation_list <- c("Malawi", "Namibia", "Nigeria", "South Africa",
                     "Uganda", "Zambia", "Zimbabwe", "India")

pick <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# Execution ####
for (i in 1:NROW(pick)) {
  print(paste0(i," of ",NROW(pick), ": ", pick[[i,1]]))

  packTool(tool = "OPU Data Pack",
           datapack_name = pick[[i,1]],
           country_uids = unlist(pick[[i,"country_uids"]]),
           template_path = NULL,
           cop_year = 2021,
           output_folder = output_folder,
           results_archive = FALSE)
}
