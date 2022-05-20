context("Test GetDatasetUids")

test_that("GetDatasetUids", {

  
  testthat::expect_error(getDatasetUids(2019))
  testthat::expect_error(suppressWarnings(getDatasetUids("foo")))

  expect_true(is.vector(getDatasetUids()))
  expect_true(all(is_uidish(getDatasetUids())))
  expect_identical(getDatasetUids(),getDatasetUids(getCurrentCOPYear()))
  
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

  testthat::expect_setequal(getDatasetUids(2022, "mer_targets"), c("iADcaCD5YXh",
                                                                   "o71WtN5JrUu",
                                                                   "vzhO50taykm")
  )
  testthat::expect_error(getDatasetUids(2022, "mer_results"))
  testthat::expect_setequal(getDatasetUids(2022, "subnat_targets"), "J4tdiDEi08O")
  testthat::expect_setequal(getDatasetUids(2022, "impatt"), "CxMsvlKepvE")
  testthat::expect_error(getDatasetUids(2022, "subnat_results"))
})
