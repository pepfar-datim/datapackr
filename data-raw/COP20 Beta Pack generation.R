library(datapackr)
library(magrittr)

secrets <- "/Users/scott/.secrets/test-mer2.json"

loginToDATIM(secrets)

#template_path <- "./data-raw/COP20_Data_Pack_Template_vTEST.xlsx"

output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/Beta Packs/Testing"

model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/SAMPLE_model_data_pack_input_jason_20_20200203_1_flat.rds"

model_data <- readRDS(model_data_path)

batch <- tibble::tribble(
  ~datapack_name, ~country_uids,
  "Kenya","HfVjCurKxh2",
  "Cote d'Ivoire","ds0ADyc9UCU",
  "Mozambique", "h11OyvlPxpJ",
  "South Africa", "cDGPF739ZZr"
)

for (i in 1:NROW(batch)) {
  packDataPack(model_data = model_data,
               datapack_name = batch[[i,1]],
               country_uids = batch[[i,2]],
               template_path = NULL,
               cop_year = 2020,
               output_folder = output_folder)
}

