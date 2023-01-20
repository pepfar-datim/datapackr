# library(datapackr)
# library(dplyr)
# library(tibble)
#
# psnuxim_model_data <- readRDS("/Users/faustolopezbao/Documents/psnuxim2023Models/PSNUxIM_COP22_2022-11-07 (1).rds")
#

testthat::test_that("can test prepare_model_data.PSNUXIM...", {

  dummy_snuxim_data <-
    list(
    )

  # ta row
  dummy_snuxim_data$XOivy2uDpMF <-
    tibble::tribble(
      ~"indicator_code", ~"psnu_uid", ~"mechanism_uid", ~"mechanism_code", ~"type", ~"age_option_name",
      ~"age_option_uid", ~"sex_option_name", ~"sex_option_uid", ~"value", ~"kp_option_uid", ~"kp_option_name",
      ~"percent",

      "HTS_INDEX_COM.New.Neg.T", "bSwiBL5Y4SW", "CkJcmmaMVSX", "160000", "TA", "20-24", "GaScV37Kk29", "Female",
      "Z1EnpTPaUfq", 2.00, "NA", "NA", 1,

      "HTS_INDEX_COM.New.Neg.T", "bSwiBL5Y4SW", "CkJcmmaMVSX", "160000", "TA", "20-24", "GaScV37Kk29", "Male",
      "Qn0I5FbKQOA", 2.00, "NA", "NA", 1
    )

  # dsd row
  dummy_snuxim_data$Qh4XMQJhbk8 <-
    tibble::tribble(
      ~"indicator_code", ~"psnu_uid", ~"mechanism_uid", ~"mechanism_code", ~"type", ~"age_option_name",
      ~"age_option_uid", ~"sex_option_name", ~"sex_option_uid", ~"value", ~"kp_option_uid", ~"kp_option_name",
      ~"percent",
      "GEND_GBV.PE.T", "BDfJLRZI6JM", "qcVSyZ8L9HX", "AFR1", "DSD", "NA", "NA", "NA", "NA", NA, "NA", 89.00, 1,
      "GEND_GBV.PE.T", "bRAFS0BaYNS", "xDplpmZisgM", "81566", "DSD", "NA", "NA", "NA", "NA", NA, "NA", 41.00, 1
    )

  country_uids <- c(
    #"XOivy2uDpMF",
    "Qh4XMQJhbk8"
  )

  # test that we can filter out mechanism codes properly
  # should filter out the mechamism code that is a letter
  res <- prepare_model_data.PSNUxIM(dummy_snuxim_data, country_uids)
  testthat::expect_equal(nrow(res), 1)
  rm(res)


})
