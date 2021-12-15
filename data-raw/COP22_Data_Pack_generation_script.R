library(datapackr)
library(magrittr)

datapackr::loginToDATIM("~/.secrets/datim.json")

output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 22/3) Testing & Deployment/Beta Packs"

model_data_path <- file.choose()

model_data <- readRDS(model_data_path)

#Beta Packs ####
betapack_countries <- c("Malawi", "Namibia", "Nigeria", "South Africa",
                        "Uganda", "Zambia", "Zimbabwe", "India") 

pick <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% betapack_countries)

# Dedupe Testing ####
# pick <- datapackr::COP21_datapacks_countries %>%
#   dplyr::filter(
#     datapack_name %in% c(
#       "Colombia",
#       "Eswatini",
#       "Namibia",
#       "Zambia",
#       "South Sudan",
#       "Malawi"
#     )
#   )

# For individual testing ####
# pick <- datapackr::COP21_datapacks_countries %>%
#   dplyr::filter(
#     !datapack_name %in% c(
#       "Botswana",
#       "Cote d'Ivoire",
#       "Eswatini",
#       "Ethiopia",
#       "India",
#       "Laos",
#       "Lesotho",
#       "Malawi",
#       "Mozambique",
#       "Rwanda",
#       "Senegal",
#       "South Africa",
#       "Tanzania",
#       "Uganda",
#       "Zambia")
#   )
# i = 1

# For Production run ####
# pick <- datapackr::COP21_datapacks_countries

# Execution ####

# i = 1
for (i in 1:NROW(pick)) {
  print(paste0(i," of ",NROW(pick), ": ", pick[[i,1]]))

  packDataPack(model_data = model_data,
               datapack_name = pick$datapack_name[i],
               country_uids = unlist(pick$country_uids[i]),
               template_path = NULL,
               cop_year = 2022,
               output_folder = output_folder,
               results_archive = FALSE)
}
