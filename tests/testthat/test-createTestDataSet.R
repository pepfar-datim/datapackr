context("test-checkTestDataSet")
with_mock_api({
test_that("Can create test data...", {
  testthat::skip("Creating test data is broken for 2022-2023.")
  c_year <- 2022

  # test year stop for 2022 and up ----
  cop_years <- 2022:2023
  c_uids <- c("V0qMZH29CtN", "qllxzIjjurr")

  lapply(cop_years, function(cop_year) {
    testthat::expect_error(
      createTestDataset(country_uids = c_uids, cop_year = cop_year),
      "Not yet set up to produce a test dataset for that COP Year."
    )

  # test can return for year 2021
  output_columns <- c("orgUnit", "dataElement", "categoryOptionCombo",
                      "period", "value_type", "attributeOptionCombo", "value")
  res <- createTestDataset(
    country_uids = c_uids,
    cop_year = c_year,
    d2_session = play2361
  )

  # test names
  testthat::expect_identical(names(res), output_columns)

  # test structure of data is what is to be expected
  test_structure <- lapply(colnames(res), function(column) {
    class(res[[column]])
  })
  testthat::expect_identical(test_structure,
                             list("character", "character", "character",
                                  "character", "character", "character", "numeric"))

  # test output period is equal to cop year fiscal year
  testthat::expect_identical(
    sort(
      as.numeric(
        unique(
          substr(
            res$period, 1, 4
          )
        )
      )
    ),
    c(c_year - 1, c_year)
  )

  # test only valid org units are included in output
  org_units <- getValidOrgUnits(c_year) %>%
    dplyr::filter(
      country_uid %in% c_uids,
      !is.na(org_type)) %>%
    dplyr::select(orgUnit = uid) %>%
    dplyr::distinct()
  testthat::expect_true(unique(org_units$orgUnit %in% res$orgUnit))


  })

})
