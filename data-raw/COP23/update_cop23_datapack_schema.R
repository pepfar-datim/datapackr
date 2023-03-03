## If you've made any edits to the Excel template, rebuild package first
## (Cmd+Shift+B) to capture these, then run the below.

library(datapackr)

datapack_template_filepath <- system.file("extdata",
                                          "COP23_Data_Pack_Template.xlsx",
                                          package = "datapackr",
                                          mustWork = TRUE)
cop23_data_pack_schema <-
  unPackSchema(
    template_path = datapack_template_filepath,
    cop_year = 2023)

waldo::compare(datapackr::cop23_data_pack_schema, cop23_data_pack_schema)

checkSchema(schema = datapackr::cop23_data_pack_schema,
            template_path = datapack_template_filepath,
            cop_year = 2023,
            tool = "Data Pack")
# TODO: Fix issue here where checkSchema flags issues in package schema as if they were in the template

save(cop23_data_pack_schema,
     file = "./data/cop23_data_pack_schema.rda",
     compress = "xz")

## Rebuild package again. (Cmd+Shift+B)
