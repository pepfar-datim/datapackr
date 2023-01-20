context("Test getCOPDatasetUids")

test_that("getCOPDatasetUids", {


  expect_error(getCOPDatasetUids(2018))
  expect_error(getCOPDatasetUids(c(2021, 2022)))
  expect_error(getCOPDatasetUids("foo"))
  expect_error(getCOPDatasetUids("foo", NULL))

  expect_true(is.vector(getDatasetUids(cop_year = 2021)))
  expect_true(all(is_uidish(getDatasetUids(cop_year = 2021))))
  #expect_identical(getDatasetUids(), getDatasetUids(getCurrentCOPYear()))

  testthat::expect_setequal(getDatasetUids(2021, "mer_targets"), c("YfZot37BbTm",
                                                                   "cihuwjoY5xP",
                                                                   "wvnouBMuLuE")
  )
  testthat::expect_setequal(getDatasetUids(2021, "mer_results"), c("BHlhyPmRTUY",
                                                                   "HfhTPdnRWES",
                                                                   "MGNVwVicMVm"))
  testthat::expect_setequal(getDatasetUids(2021, "subnat_targets"), "Va7TYyHraRn")
  testthat::expect_setequal(getDatasetUids(2021, "impatt"), "Zn27xns9Fmx")
  testthat::expect_setequal(getDatasetUids(2021, "subnat_results"), "IXiORiVFqIv")

  expect_true(all(is_uidish(getCOPDatasetUids())))

  expect_identical(getCOPDatasetUids(), getCOPDatasetUids(getCurrentCOPYear()))

  testGetDataSetUIDS <- function(cop_year, datastream) {
    expect_true(all(sapply(getCOPDatasetUids(cop_year, datastream), is_uidish)))
  }

  expect_true(testGetDataSetUIDS(2021, "mer_targets"))
  expect_true(testGetDataSetUIDS(2021, "subnat_targets"))
  expect_true(testGetDataSetUIDS(2021, "subnat_results"))
  expect_true(testGetDataSetUIDS(2022, "mer_targets"))
  expect_true(testGetDataSetUIDS(2022, c("mer_targets", "subnat_targets", "impatt")))


})
