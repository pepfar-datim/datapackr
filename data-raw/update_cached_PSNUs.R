# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

secrets <- "/Users/scott/.secrets/datim.json"

datapackr::loginToDATIM(secrets)

valid_PSNUs <- getPSNUs(additional_fields = "lastUpdated")

compare_diffs <- datapackr::valid_PSNUs %>%
  dplyr::full_join(valid_PSNUs, by = "psnu_uid") %>%
  dplyr::filter(is.na(psnu.x) | is.na(psnu.y))

save(valid_PSNUs, file = "./data/valid_PSNUs.rda", compress = "xz")
