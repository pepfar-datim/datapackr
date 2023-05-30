context("test-extractRawColumnData")

test_that("test can extract raw column data...", {

  d <- list()
  d$info$cop_year <- 2023
  d$info$messages <- MessageQueue()
  eid_cols <- "PMTCT_EID.D.T"
  cascade_cols <- c("Age", "Sex", "TX_CURR.Expected.T_1")
  non_pivot_cols <- c("psnu", "psnu_uid", "age", "sex", "key_population")

  d$sheets$EID <-
    tribble(
      ~Prioritization, ~PSNU, ~PMTCT_EID.D.T,
      "Military",       "_Military Rwanda [OAIa0wJPpZ2]", "29",
      "7 - Attained",   "East [h420jGWIrGG]",             "595",
      "7 - Attained",   "Kigali City [tuBMCugGRaW]",      "873"
    )

  d$sheets$Cascade <-
    tribble(
      ~Prioritization, ~PSNU, ~Age, ~Sex, ~ID, ~TX_CURR.Expected.T_1,
      "Military", "_Military Mozambique [siMZUtd2cJW]", "<01", "Female",
      "_Military Mozambique [siMZUtd2cJW]|<01|Female", "20",
      "Military", "_Military Mozambique [siMZUtd2cJW]", "<01",
      "Male", "_Military Mozambique [siMZUtd2cJW]|<01|Male", "13",
      "Military", "_Military Mozambique [siMZUtd2cJW]", "01-09",
      "Female", "_Military Mozambique [siMZUtd2cJW]|01-09|Female", "281"
    )

  # test warning returned when sheet null or not in list
  testthat::expect_warning(extractRawColumnData(d, NULL, eid_cols))
  testthat::expect_warning(extractRawColumnData(d, "Kp", eid_cols))

  # test PSNU is added - should add PSNU to sheet vector and pass normally
  testthat::expect_no_error(extractRawColumnData(d, "EID", eid_cols))
  testthat::expect_no_error(extractRawColumnData(d, "Cascade", cascade_cols))

  # test expected columns have been added (age, sex, key_population)
  res <- extractRawColumnData(d, "EID", eid_cols)
  testthat::expect_true(
    unique(c("age", "sex", "key_population") %in% names(res))
  )
  res <- extractRawColumnData(d, "Cascade", cascade_cols)
  testthat::expect_true(
    unique(c("age", "sex", "key_population") %in% names(res))
  )

  # test col input becomes value in output (excluding age, sex etc)
  # and is only value in indicator code -- note numeric input is class character
  res <- extractRawColumnData(d, "EID", eid_cols)
  testthat::expect_identical(d$sheets$EID$PMTCT_EID.D.T, as.character(res$value))
  testthat::expect_true(
    unique(
      res$indicator_code %in% "PMTCT_EID.D.T"
    )
  )

  res <- extractRawColumnData(d, "Cascade", cascade_cols)
  testthat::expect_identical(d$sheets$Cascade$TX_CURR.Expected.T_1, as.character(res$value))
  testthat::expect_true(
    unique(
      res$indicator_code %in% "TX_CURR.Expected.T_1"
    )
  )

})
