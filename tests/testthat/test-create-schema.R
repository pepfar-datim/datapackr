context("Create Datapack Schemas")

#Explicitly set the locale to avoid any issues related to ordering within objects
#when comparing them.

Sys.setlocale(category = "LC_COLLATE", locale = "en_US.UTF-8")

with_mock_api({
  test_that("We can create a COP23 DataPack Schema", {
    template_file <- getTemplate("COP23_Data_Pack_Template.xlsx")
    expect_true(file.exists(template_file))
    test_dataset <-   unPackSchema(
      template_path = template_file,
      skip = skip_tabs(tool = "Data Pack Template", cop_year = 2023),
      cop_year = 2023)
    expect_type(test_dataset, "list")
    expect_named(
      test_dataset,
      c(
        "sheet_num",
        "sheet_name",
        "data_structure",
        "col",
        "indicator_code",
        "dataset",
        "col_type",
        "value_type",
        "dataelement_dsd",
        "dataelement_ta",
        "categoryoption_specified",
        "valid_ages",
        "valid_sexes",
        "valid_kps",
        "formula",
        "FY",
        "period"
      ),
      ignore.order = TRUE
    )
    expect_type(test_dataset$sheet_num, "integer")
    expect_type(test_dataset$sheet_name, "character")
    expect_type(test_dataset$data_structure, "character")
    expect_type(test_dataset$col, "integer")
    expect_type(test_dataset$indicator_code, "character")
    expect_type(test_dataset$dataset, "character")
    expect_type(test_dataset$col_type, "character")
    expect_type(test_dataset$value_type, "character")
    expect_type(test_dataset$dataelement_dsd, "character")
    expect_type(test_dataset$dataelement_ta, "character")
    expect_type(test_dataset$categoryoption_specified, "character")
    expect_type(test_dataset$valid_ages, "list")
    expect_type(test_dataset$valid_sexes, "list")
    expect_type(test_dataset$valid_kps, "list")
    expect_type(test_dataset$formula, "character")
    expect_type(test_dataset$FY, "double")
    expect_type(test_dataset$period, "character")
    #load(getRDA("cop23_data_pack_schema"))
    expect_identical(test_dataset, cop23_data_pack_schema)
    # Seems to no longer work with list columns
  })

})



with_mock_api({
  test_that("Can detect when template and schema do not match", {
    template_file <- test_sheet("COP23_Data_Pack_Template_v1.xlsx")
    expect_true(file.exists(template_file))
    test_dataset <-   unPackSchema(
      template_path = template_file,
      skip = skip_tabs(tool = "Data Pack Template", cop_year = 2023),
      cop_year = 2023)
    expect_type(test_dataset, "list")
    expect_named(
      test_dataset,
      c(
        "sheet_num",
        "sheet_name",
        "data_structure",
        "col",
        "indicator_code",
        "dataset",
        "col_type",
        "value_type",
        "dataelement_dsd",
        "dataelement_ta",
        "categoryoption_specified",
        "valid_ages",
        "valid_sexes",
        "valid_kps",
        "formula",
        "FY",
        "period"
      ),
      ignore.order = TRUE
    )
    expect_type(test_dataset$sheet_num, "integer")
    expect_type(test_dataset$sheet_name, "character")
    expect_type(test_dataset$data_structure, "character")
    expect_type(test_dataset$col, "integer")
    expect_type(test_dataset$indicator_code, "character")
    expect_type(test_dataset$dataset, "character")
    expect_type(test_dataset$col_type, "character")
    expect_type(test_dataset$value_type, "character")
    expect_type(test_dataset$dataelement_dsd, "character")
    expect_type(test_dataset$dataelement_ta, "character")
    expect_type(test_dataset$categoryoption_specified, "character")
    expect_type(test_dataset$valid_ages, "list")
    expect_type(test_dataset$valid_sexes, "list")
    expect_type(test_dataset$valid_kps, "list")
    expect_type(test_dataset$formula, "character")
    expect_type(test_dataset$FY, "double")
    expect_type(test_dataset$period, "character")
    #load(getRDA("cop23_data_pack_schema"))
    expect_false(identical(test_dataset, cop23_data_pack_schema))


  })

})
