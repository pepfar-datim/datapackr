context("Indicator code tests")

test_that("We can  pass valid indicator codes", {
  d <- list()
  d$info$cop_year <- "2024"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$data$extract <- data.frame(indicator_code = c("TX_CURR.T", "TX_NEW.T"))
  d <- checkInvalidIndicatorCodes(d)
  expect_false(is.null(d$tests$invalid_indicator_codes))
  expect_false(d$info$has_error)
})


test_that("We can flag invvalid indicator codes", {
  d <- list()
  d$info$cop_year <- "2024"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$data$extract <- data.frame(indicator_code = c("TX_CURR.T", "TX_NEW.T", "FOOBAR"))
  d <- checkInvalidIndicatorCodes(d)
  expect_false(is.null(d$tests$invalid_indicator_codes))
  expect_true(grepl("FOOBAR", d$tests$invalid_indicator_codes$indicator_code))
  expect_true(grepl("ERROR! INVALID INDICATOR CODES:", d$info$messages$message))
  expect_true(d$info$has_error)
})
