# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

secrets <- "~/.secrets/datim.json"

datapackr::loginToDATIM(secrets)

valid_PSNUs <- getPSNUs(additional_fields = "lastUpdated") %>%
  dplyr::bind_rows(
    dplyr::filter(datapackr::valid_PSNUs,
                  psnu %in% c("Sierra Leone", "Burkina Faso")
                  )
    )

compare_diffs <- datapackr::valid_PSNUs %>%
  dplyr::full_join(valid_PSNUs, by = "psnu_uid") %>%
  dplyr::filter(is.na(psnu.x) | is.na(psnu.y))

save(valid_PSNUs, file = "./data/valid_PSNUs.rda", compress = "xz")


