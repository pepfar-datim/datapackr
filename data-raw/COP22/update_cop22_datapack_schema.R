## If you've made any edits to the Excel template, rebuild package first to
## capture these, then run the below.

library(datapackr)
library(magrittr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

datapack_template_filepath <- system.file("extdata",
                                          "COP22_Data_Pack_Template.xlsx",
                                          package = "datapackr",
                                          mustWork = TRUE)
cop22_data_pack_schema <-
  unPackSchema(
    template_path = datapack_template_filepath,
    skip = skip_tabs(tool = "Data Pack Template", cop_year = 2022),
    cop_year = 2022)

waldo::compare(cop22_data_pack_schema, datapackr::cop22_data_pack_schema)

save(cop22_data_pack_schema,
     file = "./data/cop22_data_pack_schema.rda",
     compress = "xz")

## Rebuild package again.