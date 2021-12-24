context("Test GetDatasetUids")

test_that("GetDatasetUids", {
  testthat::expect_equivalent(getDatasetUids(2019), c("BWBS39fydnX",
                                                      "l796jk9SW7q",
                                                      "X8sn5HE5inC",
                                                      "eyI0UOWJnDk",
                                                      "KWRj80vEfHU",
                                                      "fi9yMqWLWVy",
                                                      "zUoy5hk8r0q",
                                                      "PyD4x9oFwxJ",
                                                      "EbZrNIkuPtc",
                                                      "Ncq22MRC6gd",
                                                      "iJ4d5HdGiqG",
                                                      "pTuDWXzkAkJ")
  )
  testthat::expect_equivalent(getDatasetUids(2020), c("sBv1dj90IX6", 
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
  testthat::expect_equivalent(getDatasetUids(2021), c("Pmc0yYAIi1t", 
                                                      "s1sxJuqXsvV",
                                                      "zL8TlPVzEBZ",
                                                      "TBcmmtoaCBC",
                                                      "qHyrHc4zwx4", 
                                                      "j7jzezIhgPj",
                                                      "xiTCzZJ2GPP",
                                                      "jxnjnBAb1VD")
  )
                                                      
  testthat::expect_error(getDatasetUids(2022))
  testthat::expect_equivalent(getDatasetUids(2022,"mer_targets"), c("YfZot37BbTm",
                                                                    "cihuwjoY5xP",
                                                                    "wvnouBMuLuE")
  )
  testthat::expect_error(getDatasetUids(2022,"mer_results"))
  testthat::expect_equivalent(getDatasetUids(2022,"subnat_targets"), "Va7TYyHraRn")
  testthat::expect_equivalent(getDatasetUids(2022,"impatt"), "Zn27xns9Fmx")
  testthat::expect_error(getDatasetUids("subnat_results"))
})
