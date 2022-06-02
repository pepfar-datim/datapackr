library(datapackr)
library(magrittr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)
# datimutils::loginToDATIM("~/.secrets/datim.json")

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "COP22 Data Packs/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")
snuxim_model_data_path <- Sys.getenv("SNUXIM_MODEL_DATA_PATH")


# Unpack Submitted Data Pack ####
d <- unPackTool(cop_year = 2022)

# model_data_path <- file.choose()
# d <- checkAnalytics(d, model_data_path)

# snuxim_model_data_path <- file.choose()
# output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 22/3) Testing & Deployment/Beta Packs"
d <- writePSNUxIM(d, snuxim_model_data_path, output_folder)
