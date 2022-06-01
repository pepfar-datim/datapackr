context("can-check negative values...")

test_that("Can flag neative values...", {

  # base object
  d <- list()
  d$info$cop_year <- "2022"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$info$schema <- data.frame(
    sheet_name = c("Prioritization", "Prioritization"),
    col_type = c("target", "result"),
    dataset = c("subnat", "subnat"),
    indicator_code = c("IMPATT.PRIORITY_SNU.T", "IMPATT.PRIORITY_SNU.T")
  )

  # variables for munging
  d$sheets$Prioritization <- data.frame(
    "SNU1" = c("_Military something", "New York", "New York"),
    "PSNU" = c("_Military something", "Something [#SNU] [e1234]", "Something [#SNU] [e1235]"),
    "IMPATT.PRIORITY_SNU.T_1" = c(NA, 1, 3),
    "IMPATT.PRIORITY_SNU.T" = c("M", "-1", 1),
    "PRIORITY_SNU.translation" = c("Military", "Scale-up: Aggressive", "Scale-up: Aggressive")
  )

  # test positive error
  d <- checkNegativeValues(d, sheet = "Prioritization")
  testthat::expect_gt(nrow(d$tests$negative_values), 0) # test the tests object
  testthat::is_more_than(d$info$messages$message, 0) # test the message object

})


test_that("Can pass valid values...", {

  # base object
  d <- list()
  d$info$cop_year <- "2022"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$info$schema <- data.frame(
    sheet_name = c("Prioritization", "Prioritization"),
    col_type = c("target", "result"),
    dataset = c("subnat", "subnat"),
    indicator_code = c("IMPATT.PRIORITY_SNU.T", "IMPATT.PRIORITY_SNU.T")
  )

  # variables for munging
  d$sheets$Prioritization <- data.frame(
    "SNU1" = c("_Military something", "New York", "New York"),
    "PSNU" = c("_Military something", "Something [#SNU] [e1234]", "Something [#SNU] [e1235]"),
    "IMPATT.PRIORITY_SNU.T_1" = c(NA, 1, 3),
    "IMPATT.PRIORITY_SNU.T" = c("M", "-1", 1),
    "PRIORITY_SNU.translation" = c("Military", "Scale-up: Aggressive", "Scale-up: Aggressive")
  )

  # test no false positive
  d$sheets$Prioritization <- d$sheets$Prioritization[d$sheets$Prioritization$IMPATT.PRIORITY_SNU.T > 0, ]
  d <- checkNegativeValues(d, sheet = "Prioritization")
  testthat::expect_null(d$tests$negative_values) # test the tests object
  testthat::is_equivalent_to(d$info$messages$message, 0) # test the message object

})
