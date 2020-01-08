library(datapackr)
library(magrittr)

secrets <- "/Users/scott/.secrets/datim.json"

loginToDATIM(secrets)

output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/Beta Packs/Testing"

model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/model_data_pack_input_20_20200107_1_flat.rds"

model_data <- readRDS(model_data_path)

batch <- tibble::tribble(
  ~datapack_name, ~country_uids,
  "Kenya","HfVjCurKxh2",
  "Cote d'Ivoire","ds0ADyc9UCU",
  "Mozambique", "h11OyvlPxpJ",
  "South Africa", "cDGPF739ZZr",
  "Malawi","lZsCb6y0KDX"
)

for (i in 1:NROW(batch)) {
  packDataPack(model_data = model_data,
               datapack_name = batch[[i,1]],
               country_uids = batch[[i,2]],
               template_path = NULL,
               cop_year = 2020,
               output_folder = output_folder)
}
