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
  # should filter out the mechanism code that has letters
  res <- prepare_model_data.PSNUxIM(dummy_snuxim_data, country_uids)
  testthat::expect_equal(nrow(res), 1)
  rm(res)



  # test that indicator codes PMTCT_EID.N.2.T and PMTCT_EID.N.12.T are NA
  # with regards to age as in line
  dummy_snuxim_data$XOivy2uDpMF %<>%
    add_row(
      indicator_code = "PMTCT_EID.N.2.T",
      psnu_uid = "E5ndVNHtINe",
      mechanism_uid = "eouPVGemcrh",
      mechanism_code = "81002",
      type = "TA",
      age_option_name = "<= 02 months",
      age_option_uid = "J4SQd7SnDi2",
      sex_option_name = "NA",
      sex_option_uid = "NA",
      value = 608,
      kp_option_uid = "NA",
      kp_option_name = NA,
      percent = 1
    ) %>%
    add_row(
      indicator_code = "PMTCT_EID.N.12.T",
      psnu_uid = "wr3nbmlplbm",
      mechanism_uid = "eouPVGemcrh",
      mechanism_code = "81002",
      type = "TA",
      age_option_name = "<= 02 months",
      age_option_uid = "J4SQd7SnDi2",
      sex_option_name = "NA",
      sex_option_uid = "NA",
      value = 257,
      kp_option_uid = "NA",
      kp_option_name = NA,
      percent = 1
    )

  country_uids <- c(
    "XOivy2uDpMF"
  )

  res <- prepare_model_data.PSNUxIM(dummy_snuxim_data, country_uids)
  testthat::expect_true(
    is.na(
      unique(res[res$indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T"), ]$Age)
      )
    )
  rm(res)

  # test end result should look as it should
  expected_res <-
    as_tibble(data.frame(
      psnu_uid = c("bSwiBL5Y4SW", "bSwiBL5Y4SW", "E5ndVNHtINe", "wr3nbmlplbm"),
      indicator_code = c("HTS_INDEX_COM.New.Neg.T", "HTS_INDEX_COM.New.Neg.T", "PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T"),
      Age = c("20-24", "20-24", NA, NA),
      Sex = c("Female", "Male", "NA", "NA"),
      KeyPop = c("NA", "NA", NA, NA),
      `160000_TA` = as.numeric(c(1, 1, NA, NA)),
      `81002_TA` = as.numeric(c(NA, NA, 1, 1)),
      `Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)` = as.numeric(c(NA, NA, NA, NA)),
      `Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)` = as.numeric(c(NA, NA, NA, NA)),
      `Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)` = as.numeric(c(NA, NA, NA, NA)),
      `DSD Dedupe Resolution (FY22)` = as.character(c(NA, NA, NA, NA)),
      `TA Dedupe Resolution (FY22)` = as.character(c(NA, NA, NA, NA)),
      `Crosswalk Dedupe Resolution (FY22)` = as.character(c(NA, NA, NA, NA)),
      `DSD Dedupe` = as.numeric(c(NA, NA, NA, NA)),
      `TA Dedupe` = as.numeric(c(NA, NA, NA, NA)),
      `Crosswalk Dedupe` = as.numeric(c(NA, NA, NA, NA))
    ))

  testthat::expect_equivalent(res, expected_res)
  rm(res, expected_res)

})
