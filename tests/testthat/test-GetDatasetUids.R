context("Test getCOPDatasetUids")

test_that("getCOPDatasetUids", {


  expect_error(getCOPDatasetUids(2018))
  expect_error(getCOPDatasetUids(c(2021, 2022)))
  expect_error(suppressWarnings(getCOPDatasetUids("foo")))
  expect_true(is.vector(getCOPDatasetUids()))
  expect_true(all(is_uidish(getCOPDatasetUids())))


  expect_identical(getCOPDatasetUids(), getCOPDatasetUids(getCurrentCOPYear()))

  expect_setequal(getCOPDatasetUids(2021, "mer_targets"), c("YfZot37BbTm",
                                                                   "cihuwjoY5xP",
                                                                   "wvnouBMuLuE")
  )
  expect_setequal(getCOPDatasetUids(2021, "mer_results"), c("BHlhyPmRTUY",
                                                                   "HfhTPdnRWES",
                                                                   "MGNVwVicMVm"))
  expect_setequal(getCOPDatasetUids(2021, "subnat_targets"), "Va7TYyHraRn")
  expect_setequal(getCOPDatasetUids(2021, "impatt"), "Zn27xns9Fmx")
  expect_setequal(getCOPDatasetUids(2021, "subnat_results"), "IXiORiVFqIv")

  expect_setequal(getCOPDatasetUids(2022, "mer_targets"), c("iADcaCD5YXh",
                                                                   "o71WtN5JrUu",
                                                                   "vzhO50taykm")
  )
  expect_error(getCOPDatasetUids(2022, "mer_results"))
  expect_setequal(getCOPDatasetUids(2022, "subnat_targets"), "J4tdiDEi08O")
  expect_setequal(getCOPDatasetUids(2022, "impatt"), "CxMsvlKepvE")
  expect_error(getCOPDatasetUids(2022, "subnat_results"))
})
