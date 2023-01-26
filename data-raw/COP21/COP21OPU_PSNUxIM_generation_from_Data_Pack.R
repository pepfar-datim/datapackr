library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)
output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "COP21 OPUs/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")
snuxim_model_data_path <- Sys.getenv("SNUXIM_MODEL_DATA_PATH")

d1 <- unPackTool()

d2 <- packTool(model_data_path = model_data_path,
               snuxim_model_data_path = snuxim_model_data_path,
               undistributed_mer_data = d1$datim$UndistributedMER,
               tool = "OPU Data Pack",
               datapack_name = d1$info$datapack_name,
               country_uids = d1$info$country_uids,
               cop_year = 2021,
               output_folder = output_folder)
