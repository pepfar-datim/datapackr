library(datapackr)
library(magrittr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")

datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "COP24 Data Packs/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")
snuxim_model_data_path <- Sys.getenv("SNUXIM_MODEL_DATA_PATH")

# Unpack Submitted Data Pack ####
d <- unPackTool(cop_year = 2025, season = "COP")

d <- checkAnalytics(d, model_data_path)

d <- writePSNUxIM(d, snuxim_model_data_path, output_folder)
