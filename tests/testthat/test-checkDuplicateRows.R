context("Check duplicated rows")


test_that("Can pass with no duplicated rows", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$schema <- cop22_data_pack_schema

  d$data$extract <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
    "fakePSNU123", "VMMC_TOTALCIRC_SUBNAT.T", "15-19", "Male", NA, 10
  )
  d <- checkDuplicateRows(d, "VMMC")
  expect_null(d$tests$duplicate_rows)
})

test_that("Can flag with duplicated rows", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$schema <- cop22_data_pack_schema

  d$data$extract <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
    "fakePSNU123", "VMMC_TOTALCIRC_SUBNAT.T", "15-19", "Male", NA, 10,
    "fakePSNU123", "VMMC_TOTALCIRC_SUBNAT.T", "15-19", "Male", NA, 20
  )
  d <- checkDuplicateRows(d, "VMMC")
  expect_true(NROW(d$tests$duplicate_rows) == 1)
  expect_true(d$info$has_error)
  expect_true(grepl("DUPLICATE ROWS", d$info$messages$message))
})
