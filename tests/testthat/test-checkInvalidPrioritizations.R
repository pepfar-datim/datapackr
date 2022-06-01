# test types ----
# can pass when there are valid prioritizations
# can flag invalid prioritizations

context("can-check invalid prioritizations ...")

test_that("Can flag invalid prioritizations...", {

  # base object
  d <- list()
  d$info$cop_year <- "2022"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$info$schema <- data.frame(
    sheet_name = c("Prioritization"),
    col_type = c("target"),
    dataset = c("subnat"),
    indicator_code = c("IMPATT.PRIORITY_SNU.T"),
    value_type = c("integer")
  )

  # variables for munging
  d$sheets$Prioritization <- data.frame(
    "SNU1" = c("_Military something", "New York", "New York"),
    "PSNU" = c("_Military something", "Something [#SNU] [e29LLJAHdD3]", "Something [#SNU] [e29LLJAHdD1]"),
    "IMPATT.PRIORITY_SNU.T_1" = c(NA, 1, 3),
    "IMPATT.PRIORITY_SNU.T" = c("M", "F", 9),
    "PRIORITY_SNU.translation" = c("Military", "Scale-up: Aggressive", "Scale-up: Aggressive")
  )

  # test positive error
  d <- checkInvalidPrioritizations(d, sheet = "Prioritization")
  testthat::expect_gt(nrow(d$tests$invalid_prioritizations), 0) # test the tests object
  testthat::is_more_than(d$info$messages$message, 0) # test the message object

})

test_that("Can pass when priotizations are valid ...", {

  # base object
  d <- list()
  d$info$cop_year <- "2022"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$info$schema <- data.frame(
    sheet_name = c("Prioritization"),
    col_type = c("target"),
    dataset = c("subnat"),
    indicator_code = c("IMPATT.PRIORITY_SNU.T"),
    value_type = c("integer")
  )

  # variables for munging
  d$sheets$Prioritization <- data.frame(
    "SNU1" = c("_Military something", "New York", "New York"),
    "PSNU" = c("_Military something", "Something [#SNU] [e29LLJAHdD3]", "Something [#SNU] [e29LLJAHdD1]"),
    "IMPATT.PRIORITY_SNU.T_1" = c(NA, 1, 3),
    "IMPATT.PRIORITY_SNU.T" = c(NA, 1, 1),
    "PRIORITY_SNU.translation" = c("Military", "Scale-up: Aggressive", "Scale-up: Aggressive")
  )

  # test no false positive
  d <- checkInvalidPrioritizations(d, sheet = "Prioritization")
  testthat::expect_null(d$tests$invalid_prioritizations) # test the tests object
  testthat::is_equivalent_to(d$info$messages$message, 0) # test the message object

})
