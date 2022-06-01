# test types ----
# can flag decimal values
# can pass integers
# TODO: can pass allowed decimals?

context("can-check decimal values...")

test_that("Can flag decimal values...", {

  # base object
  d <- list()
  d$info$cop_year <- "2022"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$info$schema <- data.frame(
    sheet_name = c("VMMC", "VMMC", "VMMC", "VMMC", "VMMC", "VMMC", "VMMC"),
    col_type = c("target", "target", "target", "target", "target", "target", "target"),
    dataset = c("subnat", "subnat", "subnat", "subnat", "mer", "mer", "mer"),
    indicator_code = c("VMMC_CIRC_SUBNAT.T_1",
                       "VMMC_TOTALCIRC_SUBNAT.T_1",
                       "VMMC_TOTALCIRC_SUBNAT.T",
                       "VMMC_CIRC_SUBNAT.T",
                       "VMMC_CIRC.Unk.T",
                       "VMMC_CIRC.Pos.T",
                       "VMMC_CIRC.Neg.T"),
    value_type = c("integer", "integer", "integer", "integer", "integer", "integer", "integer")
  )

  # variables for munging
  d$sheets$VMMC <-  data.frame(
    "SNU1" = c("_Military something", "New York", "New York"),
    "PSNU" = c("_Military something", "Something [#SNU] [e1234]", "Something [#SNU] [e1235]"),
    "IMPATT.PRIORITY_SNU.T_1" = c(NA, 1, 3),
    "IMPATT.PRIORITY_SNU.T" = c("M", "-1", 2),
    "PRIORITY_SNU.translation" = c("Military", "Scale-up: Aggressive", "Scale-up: Aggressive"),
    "VMMC_CIRC_SUBNAT.T_1" = c(NA, NA, NA),
    "VMMC_TOTALCIRC_SUBNAT.T_1" = c(NA, NA, NA),
    "VMMC_TOTALCIRC_SUBNAT.T" = c(NA, NA, NA),
    "VMMC_CIRC_SUBNAT.T" = c(NA, NA, NA),
    "VMMC_CIRC.Unk.T" = c(1.2, 5087, 3456),
    "VMMC_CIRC.Pos.T" = c(2, NA, 1),
    "VMMC_CIRC.Neg.T" = c(2413, 2098, NA)
  )

  # test positive error
  d <- checkDecimalValues(d, sheet = "VMMC")
  testthat::expect_gt(nrow(d$tests$decimal_values), 0) # test the tests object
  testthat::is_more_than(d$info$messages$message, 0) # test the message object

})

test_that("Can pass with valid values...", {

  # base object
  d <- list()
  d$info$cop_year <- "2022"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$info$schema <- data.frame(
    sheet_name = c("VMMC", "VMMC", "VMMC", "VMMC", "VMMC", "VMMC", "VMMC"),
    col_type = c("target", "target", "target", "target", "target", "target", "target"),
    dataset = c("subnat", "subnat", "subnat", "subnat", "mer", "mer", "mer"),
    indicator_code = c("VMMC_CIRC_SUBNAT.T_1",
                       "VMMC_TOTALCIRC_SUBNAT.T_1",
                       "VMMC_TOTALCIRC_SUBNAT.T",
                       "VMMC_CIRC_SUBNAT.T",
                       "VMMC_CIRC.Unk.T",
                       "VMMC_CIRC.Pos.T",
                       "VMMC_CIRC.Neg.T"),
    value_type = c("integer", "integer", "integer", "integer", "integer", "integer", "integer")
  )

  # variables for munging
  d$sheets$VMMC <-  data.frame(
    "SNU1" = c("_Military something", "New York", "New York"),
    "PSNU" = c("_Military something", "Something [#SNU] [e1234]", "Something [#SNU] [e1235]"),
    "IMPATT.PRIORITY_SNU.T_1" = c(NA, 1, 3),
    "IMPATT.PRIORITY_SNU.T" = c("M", "-1", 1),
    "PRIORITY_SNU.translation" = c("Military", "Scale-up: Aggressive", "Scale-up: Aggressive"),
    "VMMC_CIRC_SUBNAT.T_1" = c(NA, NA, NA),
    "VMMC_TOTALCIRC_SUBNAT.T_1" = c(NA, NA, NA),
    "VMMC_TOTALCIRC_SUBNAT.T" = c(NA, NA, NA),
    "VMMC_CIRC_SUBNAT.T" = c(NA, NA, NA),
    "VMMC_CIRC.Unk.T" = c(1209, 5087, 3456),
    "VMMC_CIRC.Pos.T" = c(2, NA, 1),
    "VMMC_CIRC.Neg.T" = c(2413, 2098, NA)
  )

  # test positive error
  d <- checkDecimalValues(d, sheet = "VMMC")
  testthat::expect_null(d$tests$negative_values) # test the tests object
  testthat::is_equivalent_to(d$info$messages$message, 0) # test the message object

})
