context("Test flatten data pack model for cop 19 function")

test_that("flattenDataPackModel", {
  data  <-  datapackr::fake_data_flattenDataPackModel_19
  data[["ExtraCntry1"]]  <- data[["AAAAAAAAAAA"]]
# call function with a specific country
# ensure results for just that country come back
# ensure expected columns are returned
# ensure result set has expected number of rows  
  flat_data <- datapackr:::flattenDataPackModel_19(data, "AAAAAAAAAAA")
  testthat::expect_named(flat_data, "AAAAAAAAAAA")
  flat_data$AAAAAAAAAAA %>% testthat::expect_named(c("indicator_code",
                                                     "period",
                                                     "psnu_uid",
                                                     "age_option_uid",
                                                     "sex_option_uid",
                                                     "kp_option_uid",
                                                     "value"))
  testthat::expect_equal(NROW(flat_data$AAAAAAAAAAA), 3)
  
# call function without specifying a country
# ensure both countries are returned  
  
  flat_data <- datapackr:::flattenDataPackModel_19(data)
  testthat::expect_named(flat_data, c("AAAAAAAAAAA", "ExtraCntry1"))
  
# call function with a bad country "uid"
# ensure we recieve an error
  testthat::expect_error(datapackr:::flattenDataPackModel_19(data, c("AAAAAAAAAAA", "nonsense")))
})
