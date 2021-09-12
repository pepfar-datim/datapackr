library(datapackr)
library(magrittr)

datapackr::loginToDATIM("~/.secrets/datim.json")

snuxim_model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 21/3) Testing & Deployment/Model Data/PSNUxIM_20210201_1.rds"
output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 21/3) Testing & Deployment/COP21 OPUs"
model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 21/3) Testing & Deployment/Model Data/model_data_pack_input_21_20210407_1_flat.rds"

# Unpack Submitted Data Pack ####

d <- unPackTool(tool = "OPU Data Pack", cop_year = 2021)

# d <- checkAnalytics(d,
#                    model_data_path)

# d <- writePSNUxIM(d, snuxim_model_data_path, output_folder)








# Export DATIM import files ####
  exportPackr(data = d$datim$MER,
              output_path = output_folder,
              type = "DATIM Export File",
              datapack_name = d$info$datapack_name)