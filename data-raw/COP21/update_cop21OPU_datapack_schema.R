## If you've made any edits to the Excel template, rebuild package first to
## capture these, then run the below.

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

datapack_template_filepath <- system.file("extdata",
                                          "COP21_OPU_Data_Pack_Template.xlsx",
                                          package = "datapackr",
                                          mustWork = TRUE)
cop21OPU_data_pack_schema <-
  unPackSchema(
    template_path = datapack_template_filepath,
    skip = skip_tabs(tool = "OPU Data Pack Template", cop_year = 2021),
    tool = "OPU Data Pack Template",
    cop_year = 2021)

# comparison <- diffdf::diffdf(base = datapackr::cop21_data_pack_schema,
#                              compare = cop21_data_pack_schema)
#
#
#
# comparison <- compareDF::compare_df(df_new = cop21_data_pack_schema,
#                                     df_old = datapackr::cop21_data_pack_schema,
#                                     group_col = c("sheet_num", "sheet_name", "col"))
#
# comparison_df <- comparison$comparison_df
#
# View(comparison_df)

waldo::compare(cop21OPU_data_pack_schema, datapackr::cop21OPU_data_pack_schema)

save(cop21OPU_data_pack_schema,
     file = "./data/cop21OPU_data_pack_schema.rda",
     compress = "xz")

## Rebuld package again.
