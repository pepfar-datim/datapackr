test_that("Detect bad disaggs in PSNUxIM tab", {

  d <- list()
  d$info$cop_year <- 2022
  d$info$tool <- "OPU Data Pack"
  d$info$messages <- MessageQueue()

   data <- tribble(
    ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "CXCA_SCRN.T", "134-587", "Female", NA,
    "CXCA_SCRN.T", "25-29", "Female", NA
  )

  d$data$SNUxIM <- data
  d <- checkPSNUxIM_Disaggs(d)
  expect_true(NROW(d$tests$bad_disaggs_psnuxim) == 1)
  expect_true(d$tests$bad_disaggs_psnuxim$rownum == 1)
  expect_true(d$info$has_error)
  expect_true(grepl("invalid disaggregates", d$info$messages$message))
  #Bad row should be filtered
  expect_identical(d$data$SNUxIM, data[2, ])

})
