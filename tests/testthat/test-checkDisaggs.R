test_that("Detect bad disaggs in PSNUxIM tab", {

  d <- list()
  d$info$cop_year <- 2022
  d$info$tool <- "OPU Data Pack"
  d$info$messages <- MessageQueue()

  d$data$SNUxIM <- tribble(
    ~indicator_code,~Age,~Sex,~KeyPop,
    "CXCA_SCRN.T", "134-587", "Female", NA,
    "CXCA_SCRN.T", "25-29", "Female", NA
  )

  d <- checkDisaggs(d,"PSNUxIM")
  expect_true(NROW(d$tests$bad_disaggs_psnuxim) == 1)
  expect_true(d$tests$bad_disaggs_psnuxim$rownum == 1)
  expect_true(d$info$has_error)
  expect_true(grepl("Invalid disaggregates", d$info$messages$message))
})
