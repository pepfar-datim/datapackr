context("Get OPU data from DATIM")

#Uses dataValueSets.csv-da6b12.json
with_mock_api({
  test_that("We can retreive OPU data from DATIM", {
    opu_data <- getOPUDataFromDATIM(2021,"qllxzIjjurr",d2_session = training)
    expect_setequal(names(opu_data),c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value"))
    expect_true(all(is_uidish(opu_data$dataElement)))
    expect_true(all(opu_data$period == "2021Oct"))
    expect_true(all(is_uidish(opu_data$orgUnit)))
    expect_true(all(stringr::str_detect(opu_data$attributeOptionCombo,"^[0-9]{4,6}")))
    expect_true(all(is.double(opu_data$value)))
    
    })
})    