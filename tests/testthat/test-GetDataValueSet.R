context("Test GetDataSetValue")

test_that("GetDataSetValue", {
  datapackcommons::DHISLogin_Play("2.30")
  data <- GetDataValueSet(c("dataElementGroup", "orgUnit", "startDate", "endDate", "children", "limit"), 
                  c("qfxEYY9xAl6", "O6uvpzGd5pu", "2013-01-01", "2090-01-01","true","25"), 
                  "https://play.dhis2.org/2.30/")
  testthat::expect_named(data, c("data_element",
                         "period",
                         "org_unit",
                         "category_option_combo", 
                         "attribute_option_combo",
                         "value",                 
                         "stored_by",
                         "last_updated",
                         "comment",
                         "followup",
                         "deleted"))
  testthat::expect_equal(NROW(data), 25)
  testthat::expect_error(GetDataValueSet(c("limit"), 
                                         c("25"), 
                                         "https://play.dhis2.org/2.30/", 
                                         max_attempts = 1))
  })