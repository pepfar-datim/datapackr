library(datapackr)
library(magrittr)

secrets <- "/Users/scott/.secrets/datim.json"

loginToDATIM(secrets)

template_path <- "./data-raw/COP20_Data_Pack_Template_vFINAL.xlsx"

cop_year = 2020

output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/Beta Packs/Testing"

model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/SAMPLE_model_data_pack_input_jason_20_20191127_4.rds"

model_data <- readRDS(model_data_path) %>%
  flattenDataPackModel_19()

batch <- tibble::tribble(
  ~datapack_name, ~country_uids,
  "India", "skj3e4YSiJY",
  "Thailand", "Gv5ApcpDrIB",
  "Nigeria", "PqlFzhuPcF1",
  "South Africa", "cDGPF739ZZr",
  "Zimbabwe", "a71G4Gtcttv",
  "Tanzania", "mdXu6iCbn2G",
  "Uganda", "FETQ6OmnsKB",
  "Mozambique", "h11OyvlPxpJ",
  "Ethiopia", "IH1kchw86uA"
)

for (i in 1:NROW(batch)) {
  packDataPack(model_data = model_data,
               datapack_name = batch[i,1],
               country_uids = batch[i,2],
               template_path = template_path,
               cop_year = cop_year,
               output_folder = output_folder)
}

