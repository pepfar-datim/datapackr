test_that("Can provide a data frame of data set levels", {

  expected_columns <- c("iso3", "iso4", "ou", "country_name", "country_level",
  "facility_level", "community_level", "prioritization", "cop_year", "ou_uid", "country_uid")

  expect_true(inherits(dataset_levels, "data.frame"))
  expect_named(dataset_levels, expected_columns)
  expect_true(all(stringr::str_detect(dataset_levels$iso3, "^[A-Z]{3}$")))
  expect_true(all(stringr::str_detect(dataset_levels$iso4, "^[A-Z]{3}$")))
  expect_false(all(is.na(dataset_levels$country_name)))
  expect_false(all(is.na(dataset_levels$country_level)))
  expect_true(all(dataset_levels$country_level >= 3 & dataset_levels$country_level <= 4))
  expect_true(all(dataset_levels$community_level <= 8))
  expect_true(all(dataset_levels$facility_level <= 9))
  expect_true(all(dataset_levels$prioritization <= 7))
  # expect_true(all(dataset_levels$cop_year %in% supportedCOPYears()))
  expect_true(all(supportedCOPYears() %in% dataset_levels$cop_year))
  expect_true(all(is_uidish(dataset_levels$ou_uid)))
  expect_true(all(is_uidish(dataset_levels$country_uid)))

})
