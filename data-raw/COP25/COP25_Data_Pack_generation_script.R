library(datapackr)
library(dplyr)

# Point to DATIM login secrets ####
secrets <-
  Sys.getenv("SECRETS_FOLDER") %>% paste0(., "coptest.json")
datimutils::loginToDATIM(secrets)

output_folder <-
  Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "Beta Packs/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")

# For Generating Individual Data Packs ####
generation_list <- c(
  # "Angola",
  # "Benin",
  # "Botswana",
  "Brazil",
  # "Burkina Faso",
  # "Burma",
  # "Burundi",
  "Cameroon",
  # "Colombia",
  # "Cote d'Ivoire",
  # "Democratic Republic of the Congo",
  # "Dominican Republic",
  # "El Salvador",
  # "Eswatini",
  # "Ethiopia",
  # "Ghana",
  # "Guatemala",
  # "Haiti",
  # "Honduras",
  # "India",
  # "Indonesia",
  # "Jamaica",
  # "Kazakhstan",
  # "Kenya",
  # "Kyrgyzstan",
  # "Laos",
  # "Lesotho",
  # "Liberia",
  # "Malawi",
  # "Mali",
  # "Mozambique",
  # "Namibia",
  # "Nepal",
  # "Nicaragua",
  # "Nigeria",
  # "Panama",
  # "Papua New Guinea",
  # "Peru",
  # "Philippines",
  "Rwanda"#,
  # "Senegal",
  # "Sierra Leone",
  # "South Africa",
  # "South Sudan",
  # "Tajikistan",
  # "Tanzania",
  # "Thailand",
  # "Togo",
  # "Trinidad and Tobago",
  # "Uganda",
  # "Vietnam",
  # "Zambia",
  # "Zimbabwe"
)

# test valid org units against cached ####
valid_OrgUnits <- getDataPackOrgUnits(use_cache = FALSE)

compare_diffs <- datapackr::valid_OrgUnits_2025 %>%
  dplyr::full_join(valid_OrgUnits, by = "uid") %>%
  dplyr::filter(is.na(name.x) | is.na(name.y))

if (NROW(compare_diffs) > 0) {
  stop("Valid org units are not up to date! Please update valid org units.")
} else {
  rm(valid_OrgUnits, compare_diffs)
}

# # For Production run ####
pick <- datapackr::cop25_datapack_countries %>%
  dplyr::filter(datapack_name %in% generation_list)

# Execution ####
for (i in seq_along(pick$datapack_name)) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  d <- packTool(
    model_data_path = model_data_path,
    tool = "Data Pack",
    datapack_name = pick$datapack_name[i],
    country_uids = unlist(pick$country_uids[i]),
    template_path = NULL,
    cop_year = 2025,
    output_folder = output_folder,
    results_archive = FALSE
  )
}
