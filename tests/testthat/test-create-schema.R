context("Create a DataPackSchema")


with_mock_api({
  test_that("We can create a datapack schema", {
    
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset <- unPackSchema_datapack(test_sheet('COP21_Data_Pack_Template.xlsx'))
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
})
