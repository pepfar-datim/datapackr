#TODO: List PSNU separately in another col?

library(magrittr)
library(datapackr)

secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

# NOTE: Full documentation can be found in data.R
# The current list can be viewed by running View(valid_OrgUnits)

# Fetch PSNU values
valid_OrgUnits <- getDataPackOrgUnits()

# Comparing default valid_OrgUnits list to newly modified list
compare_diffs <- datapackr::valid_OrgUnits %>%
  dplyr::full_join(valid_OrgUnits, by = "psnu_uid") %>%
  dplyr::filter(is.na(name.x) | is.na(name.y))

# Overwriting default list with newly created list
save(valid_OrgUnits,
     file = "./data/valid_OrgUnits.rda",
     compress = "xz")
