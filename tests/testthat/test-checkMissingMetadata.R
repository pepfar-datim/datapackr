context("can-check missing meta data...")


test_that("Can check missing meta data in all sheets", {
  # base object
  d <- list()
  d$info$cop_year <- "2021"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE

  # test no false positive
  d$sheets$Prioritization <-
    data.frame(matrix(ncol = 5, nrow = 0))
  cols <-
    c(
      "SNU1",
      "PSNU",
      "IMPATT.PRIORITY_SNU.T_1",
      "IMPATT.PRIORITY_SNU.T",
      "PRIORITY_SNU.translation"
    )
  colnames(d$sheets$Prioritization) <- cols
  d <- checkMissingMetadata(d, sheet = "Prioritization")
  testthat::expect_identical(d$info$messages$message, character(0))

  # test positive error
  err <-
    data.frame(
      "SNU1" = NA,
      "PSNU" = NA,
      "IMPATT.PRIORITY_SNU.T_1" = NA,
      "IMPATT.PRIORITY_SNU.T" = NA,
      "PRIORITY_SNU.translation" = NA
    )
  d$sheets$Prioritization <- rbind(d$sheets$Prioritization, err)
  d <- checkMissingMetadata(d, sheet = "Prioritization")
  testthat::is_more_than(d$info$messages$message, 0)

})
