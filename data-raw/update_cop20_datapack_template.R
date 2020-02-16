## If you've made any edits to the Excel template, rebuild package first to
## capture these, then run the below.

secrets <- "/Users/scott/.secrets/test-mer2.json"

loginToDATIM(secrets)

datapack_template_filepath <- system.file("extdata",
                                          "COP20_Data_Pack_Template_vFINAL.xlsx",
                                          package = "datapackr",
                                          mustWork = TRUE)
cop20_data_pack_schema <-
  unPackSchema_datapack(
    filepath = datapack_template_filepath,
    skip = skip_tabs(tool = "Data Pack Template", cop_year = 2020),
    cop_year = 2020)

save(cop20_data_pack_schema,
     file = "./data/cop20_data_pack_schema.rda",
     compress = "xz")

## Rebuld package again.
