context("Test GetDataValueSets")

test_that("GetDataValueSets", {
# httptest::start_capturing(simplify = FALSE)
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.36.1/api/36/dataValueSets.csv?",
#   "dataElementGroup=qfxEYY9xAl6&orgUnit=O6uvpzGd5pu&startDate=2013-01-01",
#   "&endDate=2090-01-01&children=true&limit=25"),
#   httr::authenticate("admin", "district")))
# httptest::stop_capturing()

  with_mock_api({
    expect_silent(data <- getDataValueSets(c("dataElementGroup", "orgUnit", "startDate", "endDate", "children", "limit"),
                                           c("qfxEYY9xAl6", "O6uvpzGd5pu", "2013-01-01", "2090-01-01","true","25"),
                                           api_version = 36,
                                           d2_session = play2361))
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
})
