# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

secrets <- "~/.secrets/datim.json"

datimutils::loginToDATIM(secrets)

# NOTE: Full documentation can be found in data.R
# The current list can be viewed by running View(valid_PSNUs)

# Fetch PSNU values
valid_PSNUs <- getPSNUs(additional_fields = "lastUpdated")

# Comparing default valid_PSNUs list to newly modified list
compare_diffs <- datapackr::valid_PSNUs %>%
  dplyr::full_join(valid_PSNUs, by = "psnu_uid") %>%
  dplyr::filter(is.na(psnu.x) | is.na(psnu.y))

# Overwriting default list with newly created list
save(valid_PSNUs,
     file = "./data/valid_PSNUs.rda",
     compress = "xz")