## If you've made any edits to the Excel template, rebuild package first
## (Cmd+Shift+B) to capture these, then run the below.

library(datapackr)

datapack_template_filepath <- rprojroot::find_package_root_file("inst/extdata/COP25_PSNUxIM_Template.xlsx")

cop24_psnuxim_schema <-
  unPackSchema(
    template_path = datapack_template_filepath,
    tool = "PSNUxIM Tool Template",
    cop_year = 2025)

waldo::compare(datapackr::cop25_psnuxim_schema, cop25_psnuxim_schema)

#TODO: May need to redo this whole process for the cop23_psnuxim_schema too

checkSchema(schema = cop25_psnuxim_schema,
            template_path = datapack_template_filepath,
            cop_year = 2025,
            tool = "PSNUxIM Tool")

usethis::use_data(cop25_psnuxim_schema, overwrite = TRUE, compress = "xz")

## Rebuild package again. (Cmd+Shift+B)
