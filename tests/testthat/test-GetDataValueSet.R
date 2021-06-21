context("Test GetDataValueSets")


#I think this should be using a mock, otherwise, its easy that it is going to fail. 

test_that("GetDataValueSets", {
  datimutils::loginToDATIM(username = "admin", 
                           password = "district",
                           base_url = "https://play.dhis2.org/2.34.4/",
                           d2_session_name = "play")
  expect_silent(data <- datapackr:::getDataValueSets(c("dataElementGroup", "orgUnit", "startDate", "endDate", "children", "limit"), 
                  c("qfxEYY9xAl6", "O6uvpzGd5pu", "2013-01-01", "2090-01-01","true","25"), 
                  d2_session = play))
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
  testthat::expect_error(getDataValueSets(c("limit"), 
                                         c("25"), 
                                         base))
  })