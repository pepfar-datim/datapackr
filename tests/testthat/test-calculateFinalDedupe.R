context("Calculation of final dedupe values")

test_that("Do not modify valid  DSD dedupe", {

  header_cols <- list()

  header_cols$indicator_code <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop")

  d <- list()

  test_data <- tibble::tribble(
    ~PSNU, ~ indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~mechCode_supportType, ~value,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "12345_DSD", 10,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "98765_DSD", 10,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "DSD Dedupe", -10
  )

  d$data$SNUxIM <- test_data

   d <- calculateFinalDedupeValues(d, header_cols)

   expect_true(identical(d$data$SNUxIM, test_data)) })


test_that("Impute DSD dedupe value as zero when is NA 2 mechs", {

  header_cols <- list()

  header_cols$indicator_code <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop")

  d <- list()

  test_data <- tibble::tribble(
    ~PSNU, ~ indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~mechCode_supportType, ~value,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "12345_DSD", 10,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "98765_DSD", 10,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "DSD Dedupe", NA
  )

  d$data$SNUxIM <- test_data

  d <- calculateFinalDedupeValues(d, header_cols)

  expect_true(d$data$SNUxIM %>%
    dplyr::filter(mechCode_supportType == "DSD Dedupe") %>%
    dplyr::pull(value) == 0)


  })


test_that(" Do not impute DSD dedupe value as zero when is NA 1 mech", {

  header_cols <- list()

  header_cols$indicator_code <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop")

  d <- list()

  test_data <- tibble::tribble(
    ~PSNU, ~ indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~mechCode_supportType, ~value,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "12345_DSD", 10,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "DSD Dedupe", NA
  )

  d$data$SNUxIM <- test_data

  d <- calculateFinalDedupeValues(d, header_cols)

  #NA values will be dropped
  expect_true(identical(test_data[1, ], d$data$SNUxIM))


})


test_that("Impute Crosswalk dedupe value as zero when is NA 2 mechs", {

  header_cols <- list()

  header_cols$indicator_code <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop")

  d <- list()

  test_data <- tibble::tribble(
    ~PSNU, ~ indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~mechCode_supportType, ~value,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "12345_DSD", 10,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "98765_TA", 10,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "DSD Dedupe", NA,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "TA Dedupe", NA,
    "Fish District", "TX_CURR", "25-49", "Male", NA, "abc123", "Crosswalk Dedupe", NA
  )

  d$data$SNUxIM <- test_data

  d <- calculateFinalDedupeValues(d, header_cols)

  expect_true(d$data$SNUxIM %>%
                dplyr::filter(mechCode_supportType == "Crosswalk Dedupe") %>%
                dplyr::pull(value) == 0)


  expect_false(all(grepl("DSD Dedupe", d$data$SNUxIM$mechCode_supportType)))
  expect_false(all(grepl("TA Dedupe", d$data$SNUxIM$mechCode_supportType)))

})
