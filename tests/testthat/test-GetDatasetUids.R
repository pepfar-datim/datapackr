context("Test GetDatasetUids")

test_that("GetDatasetUids", {

  testthat::expect_setequal(getDatasetUids(2019), c("sBv1dj90IX6",
                                                    "nIHNMxuPUOR",
                                                    "C2G7IyPPrvD",
                                                    "HiJieecLXxN",
                                                    "qzVASYuaIey",
                                                    "BPEyzcDb8fT",
                                                    "jKdHXpBfWop",
                                                    "em1U5x9hhXh",
                                                    "mbdbMiLZ4AA",
                                                    "N4X89PgW01w",
                                                    "ctKXzmv2CVu",
                                                    "pTuDWXzkAkJ")
  )

  testthat::expect_setequal(getDatasetUids(2020), c("s1sxJuqXsvV",
                                                    "Pmc0yYAIi1t",
                                                    "zL8TlPVzEBZ",
                                                    "TBcmmtoaCBC",
                                                    "qHyrHc4zwx4",
                                                    "j7jzezIhgPj",
                                                    "xiTCzZJ2GPP",
                                                    "jxnjnBAb1VD")
  )

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
  #Not certain that throwing an error here is actually warranted?
  #testthat::expect_error(getDatasetUids(2022))
  testthat::expect_setequal(getDatasetUids(2022, "mer_targets"), c("iADcaCD5YXh",
                                                                   "o71WtN5JrUu",
                                                                   "vzhO50taykm")
  )
  testthat::expect_error(getDatasetUids(2022, "mer_results"))
  testthat::expect_setequal(getDatasetUids(2022, "subnat_targets"), "J4tdiDEi08O")
  testthat::expect_setequal(getDatasetUids(2022, "impatt"), "CxMsvlKepvE")
  testthat::expect_error(getDatasetUids(2022, "subnat_results"))
})
