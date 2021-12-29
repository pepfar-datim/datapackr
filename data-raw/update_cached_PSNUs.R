# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

secrets <- "~/.secrets/datim.json"

datapackr::loginToDATIM(secrets)


# NOTE: Full documentation can be found in data.R
# The current list can be viewed by running View(valid_PSNUs)

# NOTE:
# Burkina Faso and Sierra Leone changed PSNU levels from Country to SNU1
# between COP20 and COP21.
# We preserve their country level PSNUs for the moment to allow for COP20 OPU
# Processing

# Fetch PSNU values, filter PSNUs as needed, Stack with default valid_PSNUs list
valid_PSNUs <- getPSNUs(additional_fields = "lastUpdated") %>%
  dplyr::bind_rows(
    dplyr::filter(datapackr::valid_PSNUs,
                  psnu %in% c("Sierra Leone", "Burkina Faso")
                  )
    )


# Comparing default valid_PSNUs list to newly modified list
compare_diffs <- datapackr::valid_PSNUs %>%
  dplyr::full_join(valid_PSNUs, by = "psnu_uid") %>%
  dplyr::filter(is.na(psnu.x) | is.na(psnu.y))


# Overwriting default list with newly created list
save(valid_PSNUs,
     file = "./data/valid_PSNUs.rda",
     compress = "xz") 

