# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

secrets <- "~/.secrets/datim.json"

datapackr::loginToDATIM(secrets)

# Burkina Faso and Sierra Leone changed PSNU levels from Country to SNU1
# between COP20 and COP21.
# We preserve their country level PSNUs for the moment to allow for COP20 OPU
# Processing
valid_PSNUs <- getPSNUs(additional_fields = "lastUpdated")

compare_diffs <- datapackr::valid_PSNUs %>%
  dplyr::full_join(valid_PSNUs, by = "psnu_uid") %>%
  dplyr::filter(is.na(psnu.x) | is.na(psnu.y))

save(valid_PSNUs, file = "./data/valid_PSNUs.rda", compress = "xz")