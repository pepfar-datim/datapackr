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
cop22OPU_data_pack_schema <-
  unPackSchema(
    template_path = datapack_template_filepath,
    skip = skip_tabs(tool = "OPU Data Pack Template", cop_year = 2022),
    tool = "OPU Data Pack Template",
    cop_year = 2022)

waldo::compare(datapackr::cop22OPU_data_pack_schema,
               cop22OPU_data_pack_schema,
               max_diffs = Inf)

save(cop22OPU_data_pack_schema,
     file = "./data/cop22OPU_data_pack_schema.rda",
     compress = "xz")

## Rebuild package again.
