context("Check defunt disaggs")

test_that("Can error on trying to check an SNUxIM tab", {

  d <- list()
  expect_error(defunctDisaggs(d, "SNUxIM", "Sorry! Can't check the SNU x IM tab with this function."))
})

test_that("Can pass valid disaggs", {

  d <- list()
  d$info$schema <- cop22_data_pack_schema
  d$data$extract <- tibble::tribble(
    ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "VMMC_TOTALCIRC_SUBNAT.T", "15-19", "Male", NA
  )
   d <- defunctDisaggs(d, "VMMC")
   expect_true(NROW(d$tests$defunct_disaggs) == 0)
})

test_that("Can flag invalid disaggs", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$info$schema <- cop22_data_pack_schema
  d$data$extract <- tibble::tribble(
    ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "VMMC_TOTALCIRC_SUBNAT.T", "15-19", "Female", NA
  )
  d <- defunctDisaggs(d, "VMMC")
  expect_true(NROW(d$tests$defunct_disaggs) == 1)
  expect_true(grepl("INVALID DISAGGS", d$info$messages$message))
  expect_true(d$info$has_error)
})
