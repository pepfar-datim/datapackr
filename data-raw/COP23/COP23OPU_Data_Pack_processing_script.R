library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "COP23 OPUs/")
# model_data_path <- Sys.getenv("MODEL_DATA_PATH")
# snuxim_model_data_path <- Sys.getenv("SNUXIM_MODEL_DATA_PATH")

# Unpack Submitted Data Pack ####
d <- unPackTool(tool = "OPU Data Pack", cop_year = 2023)

# Export DATIM import files ####
exportPackr(data = d$datim$MER,
            output_folder = output_folder,
            type = "DATIM Export File",
            datapack_name = d$info$datapack_name)
