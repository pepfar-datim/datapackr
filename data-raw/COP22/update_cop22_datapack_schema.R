## If you've made any edits to the Excel template, rebuild package first to
## capture these, then run the below.

library(datapackr)
library(magrittr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

template_file <- rprojroot::find_package_root_file("inst/extdata/COP22_Data_Pack_Template.xlsx")
cop22_data_pack_schema <-
  unPackSchema(
    template_path = template_file,
    cop_year = 2022)

waldo::compare(cop22_data_pack_schema, datapackr::cop22_data_pack_schema)

usethis::use_data(cop22_data_pack_schema, overwrite = TRUE, compress = "xz")

## Rebuild package again.
