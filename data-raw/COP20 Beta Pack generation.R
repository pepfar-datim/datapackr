library(datapackr)
library(magrittr)

secrets <- "/Users/scott/.secrets/datim.json"

loginToDATIM(secrets)

datapack_name <- "Uganda"
country_uids <- c("FETQ6OmnsKB")
                #Uganda         #Tanzania       #Ghana

template_path <- NA

cop_year = 2020

output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/2) Development"

model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/SAMPLE_model_data_pack_input_jason_20_20191127_4.rds"

model_data <- readRDS(model_data_path) %>%
  flattenDataPackModel_19()


# devtools::install_github(repo = "https://github.com/pepfar-datim/data-pack-commons.git", ref = "Prod")
# library(datapackcommons)

packDataPack(model_data,
             datapack_name,
             country_uids,
             template_path,
             cop_year,
             output_folder)