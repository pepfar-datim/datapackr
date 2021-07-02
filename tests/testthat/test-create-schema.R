context("Create a DataPackSchema")


with_mock_api({
  test_that("We can create a datapack schema", {

    test_dataset <- unPackSchema_datapack(test_sheet('COP21_Data_Pack_Template.xlsx'),
                                          d2_session = training)
    expect_type(test_dataset, "list")
    expect_named(
      test_dataset,
      c(
        'sheet_num',
        'sheet_name',
        'data_structure',
        'col',
        'indicator_code',
        'dataset',
        'col_type',
        'value_type',
        'dataelement_dsd',
        'dataelement_ta',
        'categoryoption_specified',
        'valid_ages',
        'valid_sexes',
        'valid_kps',
        'formula',
        'FY',
        'period'
      ),
      ignore.order = TRUE
    )
    expect_type(test_dataset$sheet_num,"integer")
    expect_type(test_dataset$sheet_name,"character")
    expect_type(test_dataset$data_structure,"character")
    expect_type(test_dataset$col,"integer")
    expect_type(test_dataset$indicator_code,"character")
    expect_type(test_dataset$dataset,"character")
    expect_type(test_dataset$col_type,"character")
    expect_type(test_dataset$value_type,"character")
    expect_type(test_dataset$dataelement_dsd,"character")
    expect_type(test_dataset$dataelement_ta,"character")
    expect_type(test_dataset$categoryoption_specified,"character")
    expect_type(test_dataset$valid_ages,"list")
    expect_type(test_dataset$valid_sexes,"list")
    expect_type(test_dataset$valid_kps,"list")
    expect_type(test_dataset$formula,"character")
    expect_type(test_dataset$FY,"double")
    expect_type(test_dataset$period,"character")
  })

  test_that("COP21 template and schema match", {
      expect_identical(unPackSchema_datapack(
        filepath = cop21_datapack_template_path,
        skip = skip_tabs(tool = "Data Pack Template", cop_year = 2021),
        cop_year = 2021,
        d2_session = training),
        datapackr::cop21_data_pack_schema
        )
  })
  
  # test_that("COP20 template and schema match", {
  #   
  #   test_dataset <- expect_identical(
  #     unPackSchema_datapack(cop20_datapack_template_path, d2_session = training),
  #     cop20_data_pack_schema
  #   )
  # })
  # test_that("COP20 opu template and schema match", {
  #   
  #   test_dataset <- expect_identical(
  #     unPackSchema_datapack(cop20_opu_datapack_template_path, d2_session = training),
  #     datapackr::cop20OPU_data_pack_schema
  #   )
  # })
})
