## If you've made any edits to the Excel template, rebuild package first
## (Cmd+Shift+B) to capture these, then run the below.

library(datapackr)

datapack_template_filepath <- rprojroot::find_package_root_file("inst/extdata/COP24_Data_Pack_Template.xlsx")

cop24_data_pack_schema <-
  unPackSchema(
    template_path = datapack_template_filepath,
    cop_year = 2024)

waldo::compare(datapackr::cop24_data_pack_schema, cop24_data_pack_schema, max_diffs = Inf)

checkSchema(schema = cop24_data_pack_schema,
            template_path = datapack_template_filepath,
            cop_year = 2024,
            tool = "Data Pack")

usethis::use_data(cop24_data_pack_schema, overwrite = TRUE, compress = "xz")

## Rebuild package again. (Cmd+Shift+B)
