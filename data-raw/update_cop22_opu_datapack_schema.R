## If you've made any edits to the Excel template, rebuild package first to
## capture these, then run the below.

library(datapackr)
library(magrittr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

datapack_template_filepath <- system.file("extdata",
                                          "COP22_OPU_Data_Pack_Template.xlsx",
                                          package = "datapackr",
                                          mustWork = TRUE)
cop22opu_data_pack_schema <-
  unPackSchema_datapack(
    template_path = datapack_template_filepath,
    skip = skip_tabs(tool = "OPU Data Pack Template", cop_year = 2022),
    cop_year = 2022)

waldo::compare(cop22opu_data_pack_schema, datapackr::cop22opu_data_pack_schema)

save(cop22opu_data_pack_schema,
     file = "./data/cop22opu_data_pack_schema.rda",
     compress = "xz")

## Rebuild package again.
