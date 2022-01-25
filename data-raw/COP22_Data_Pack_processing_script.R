library(datapackr)
library(magrittr)

datimutils::loginToDATIM("~/.secrets/datim.json")

# Unpack Submitted Data Pack ####
d <- unPackTool(cop_year = 2022)

#model_data_path <- file.choose()
# d <- checkAnalytics(d,
#                    model_data_path)

snuxim_model_data_path <- file.choose()
output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 22/3) Testing & Deployment/Beta Packs"
d <- writePSNUxIM(d, snuxim_model_data_path, output_folder)