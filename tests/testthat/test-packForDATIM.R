context("Test Packing for all exports via packForDATIM")

test_that("Can test different combinations of model param passing", {

  # can test flag data is NULL ----
  data_error_1 <- "For type 'Undistributed MER', expected to see data from the main"
  data_error_2 <- "tabs of your Data Pack. However, this appears to be missing."
  d <- list()
  d$info$cop_year <- 2024
  testthat::expect_error(
    d <- packForDATIM(d, type = "Undistributed MER"),
    paste(data_error_1, data_error_2)
  )

  # can test missing types are flagged ----
  d <- list()
  d$info$cop_year <- 2024
  testthat::expect_error(
    d <- packForDATIM(d),
    "Specify type: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM', 'Undistributed MER'"
  )

  # can we pack MER data ----
  d <- list()
  d$info$cop_year <- 2024
  d$data$MER <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Female", NA, 354,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Male", NA, 371,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "10-14", "Female", NA, 299
    )

  d <- packForDATIM(d, type = "Undistributed MER")

  # test the right objects were created under datim
  testthat::expect_equal(
    names(d$datim),
    c("UndistributedMER")
  )

  # test the right columns are present in the datim objects that were created
  testthat::expect_equal(
    names(d$datim$UndistributedMER),
    c("dataElement", "period", "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value")
  )

  # can we pack SUBNATT data ----
  d <- list()
  d$info$cop_year <- 2024
  d$data$SUBNAT_IMPATT <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 1,
      "Centre [IaFLxtEwIwk]", "IaFLxtEwIwk", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 1,
      "Est [KtMNtCHNQDJ]", "KtMNtCHNQDJ", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 1,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "DIAGNOSED_SUBNAT.T_1", "01-09", "Female", NA, 233,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "DIAGNOSED_SUBNAT.T_1", "01-09", "Male", NA, 235
    )

  d <- packForDATIM(d, type = "SUBNAT_IMPATT")

  # test the right objects were created under datim
  testthat::expect_equal(
    names(d$datim),
    c("prioritizations", "subnat_impatt")
  )

  # test that prioritizations end up in prioritizations
  testthat::expect_equal(
    sort(unique(d$datim$prioritizations$orgUnit)),
    unique(d$data$SUBNAT_IMPATT %>%
             dplyr::filter(indicator_code == "IMPATT.PRIORITY_SNU.T") %>%
             dplyr::pull(psnuid) %>%
             sort())
  )

  # test subnat data since POP DATA SHOULD BE THERE
  # this is the result of additions of POP DATA as of
  testthat::expect_equal(
    NROW(d$datim$subnat_impatt$orgUnit),
    2
  )

  # test the right columns are present in the datim objects that were created
  lapply(names(d$datim), function(y) {
    testthat::expect_equal(
      names(d$datim[[y]]),
      c("dataElement", "period", "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value")
    )
  })

  # can we pack psnuxim data ----
  d <- list()
  d$info$cop_year <- 2024
  d$data$SNUxIM <-
    tibble::tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~mech_code, ~support_type, ~value,
      "Adamaoua [xkPoe5mhHE5]", "GEND_GBV.PE.T", NA, NA, NA, "xkPoe5mhHE5", "81580", "DSD", 150,
      "Adamaoua [xkPoe5mhHE5]", "GEND_GBV.S.T", NA, NA, NA, "xkPoe5mhHE5", "81580", "DSD", 300,
      "Adamaoua [xkPoe5mhHE5]", "HTS.Index.Neg.T", "01-09", "Female", NA, "xkPoe5mhHE5", "81580", "DSD", 485,
      "_Military Cameroon [eky41asIrS2]", "TX_TB.D.New.Neg.T", "15+", "Female", NA, "eky41asIrS2", "86656", "DSD", 239,
      "_Military Cameroon [eky41asIrS2]", "TX_TB.D.New.Neg.T", "15+", "Male", NA, "eky41asIrS2", "86656", "DSD", 179
    )

  # test mer data is not created if it already exists
  mer_data <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Female", NA, 354,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Male", NA, 371
    )

  d$data$MER <- mer_data
  d <- packForDATIM(d, type = "PSNUxIM")
  testthat::expect_equal(d$data$MER, mer_data)

  # test mer data is created when it does not exist
  d$datim <- NULL
  d$data$MER <- NULL
  d <- packForDATIM(d, type = "PSNUxIM")
  testthat::expect_equal(
    names(d$datim),
    c("MER")
  )

  # test the right columns are present in the datim objects that were created
  testthat::expect_equal(
    names(d$datim$MER),
    c("dataElement", "period", "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value")
  )

  # can we test blank rows ----
  d <- list()
  d$info$cop_year <- 2024
  d$info$messages <- MessageQueue()
  d$data$MER <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Female", NA, 354,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Male", NA, 371,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "10-14", "Female", NA, NA
    )

  d <- packForDATIM(d, type = "Undistributed MER")
  testthat::expect_true(any(grepl("ERROR! DATIM Export has blank rows", d$info$messages$message)))


})
