context("Test getCOPDatasetUids")

test_that("getCOPDatasetUids", {


  expect_error(getCOPDatasetUids(2018))
  expect_error(getCOPDatasetUids(c(2021, 2022)))
  expect_error(getCOPDatasetUids("foo"))
  expect_error(getCOPDatasetUids("foo", NULL))

  expect_true(all(is_uidish(getCOPDatasetUids(cop_year = 2024))))
  expect_true(all(is_uidish(getCOPDatasetUids())))

  expect_identical(getCOPDatasetUids(), getCOPDatasetUids(getCurrentCOPYear()))

  testGetDataSetUIDS <- function(cop_year, datastream) {
    expect_true(all(sapply(getCOPDatasetUids(cop_year, datastream), is_uidish)))
  }

  expect_true(testGetDataSetUIDS(2024, "mer_targets"))
  expect_true(testGetDataSetUIDS(2024, "subnat_targets"))
  expect_true(testGetDataSetUIDS(2024, "subnat_results"))
  expect_true(testGetDataSetUIDS(2024, "mer_targets"))
  expect_true(testGetDataSetUIDS(2024, c("mer_targets", "subnat_targets", "impatt")))

})
