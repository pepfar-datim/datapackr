#
# This script is to be used when producing COP23 OPU Tools — i.e., PSNUxIM tools
# — in cases where an OPU requires ONLY a target shift IM. Unlike the regular
# Data Pack generation process where a country generates a PSNUxIM tool via the
# Self-Service App based on data from an existing Data Pack, this code generates
# a PSNUxIM tool based on data pulled directly from DATIM.
#
# If a country needs more than a target shift among IMs — i.e., top-level target
# changes — DO NOT use this process. Instead, send back their latest Data Pack
# representing the most updated understanding of their targets as in DATIM.
#

library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "Documents/COP23 OPUs/")

# For Generating Individual Data Packs ####
pick <- datapackr::cop_datapack_countries %>%
 dplyr::filter(datapack_name %in% c("Eswatini"))

# test valid org units against cached ####
valid_OrgUnits <- getDataPackOrgUnits(use_cache = FALSE) %>%
  dplyr::filter(country_uid %in% unlist(pick$country_uids))
#TODO: Make it possible to pull and compare for a single (or list) of countries

valid_OrgUnits_package <- datapackr::valid_OrgUnits %>%
  dplyr::filter(country_uid %in% unlist(pick$country_uids))

compare_diffs <- valid_OrgUnits_package %>%
  dplyr::full_join(valid_OrgUnits, by = "uid") %>%
  dplyr::filter(is.na(name.x) | is.na(name.y))

if (NROW(compare_diffs) > 0) {
  stop("Valid org units are not up to date! Please update valid org units.")
}

waldo::compare(valid_OrgUnits_package, valid_OrgUnits)

# Execution ####
for (i in seq_along(pick$datapack_name)) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  d <- packTool(tool = "OPU Data Pack",
                datapack_name = pick$datapack_name[i],
                country_uids = unlist(pick$country_uids[i]),
                template_path = NULL,
                cop_year = 2023,
                output_folder = output_folder,
                results_archive = FALSE)
}
