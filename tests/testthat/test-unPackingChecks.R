context("test-unPackingChecks")

test_that("Can detect invalid comment types ...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = TRUE,
                    load_sheets = TRUE,
                    d2_session = training) %>%
    checkToolComments()

  expect_true(d$info$has_comments_issue)
  expect_true(d$info$has_error)


  d  <- loadDataPack(submission_path = test_sheet("COP21_Data_Pack_Template.xlsx"),
                     tool = "Data Pack",
                     country_uids = NULL,
                     cop_year = NULL,
                     load_wb = TRUE,
                     load_sheets = TRUE,
                     d2_session = training) %>%
    checkToolComments()

  # should expect no issues so FALSE
  expect_false(d$info$has_comments_issue)
})

test_that("Can detect external links in a file ...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = TRUE,
                    load_sheets = FALSE,
                    d2_session = training) %>%
    checkToolConnections()

  expect_true(d$info$has_external_links)
})

test_that("Can check Tool structure...", {
  d  <- loadDataPack(submission_path = test_sheet("COP21_Data_Pack_Template.xlsx"),
                     tool = "Data Pack",
                     country_uids = NULL,
                     cop_year = NULL,
                     d2_session = training)

  expect_equal(NROW(d$tests$missing_sheets), 0L)

  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = test_sheet("COP21_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)
  openxlsx::removeWorksheet(wb, "PMTCT")
  openxlsx::saveWorkbook(wb, file = template_copy, overwrite = TRUE)
  d <- loadDataPack(submission_path = template_copy,
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    d2_session = training)
  d <- checkToolStructure(d)
  expect_equal(NROW(d$tests$missing_sheets), 1L)
  expect_true(grepl("MISSING SHEETS", d$info$messages$message))

})




test_that("Can check Sheet structure...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkColumnStructure(d, "GEND")
  expect_true("missing_cols" %in% names(d$tests))
  expect_true("duplicate_columns" %in% names(d$tests))
  expect_true("columns_out_of_order" %in% names(d$tests))
  expect_true(any(grepl("In tab GEND, DUPLICATE COLUMNS", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND, MISSING COLUMNS", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND, OUT OF ORDER COLUMNS", d$info$messages$message)))
})

test_that("Can detect duplicate rows...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkDupeRows(d, "GEND")
  expect_true("duplicate_rows" %in% names(d$tests))
  expect_true(any(grepl("In tab GEND: DUPLICATE ROWS", d$info$messages$message)))
})

test_that("Can detect non-numeric values...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkNonNumeric(d, "GEND")
  expect_true("non_numeric" %in% names(d$tests))
  expect_true(any(grepl("In tab GEND: NON-NUMERIC VALUES", d$info$messages$message)))
})

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

test_that("Can detect negative values...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkNegativeValues(d, "GEND")
  expect_true("negative_values" %in% names(d$tests))
  expect_true(any(grepl("In tab GEND: NEGATIVE VALUES", d$info$messages$message)))
  expect_gt(nrow(d$tests$negative_values), 0) # test the tests object
})

test_that("Can detect decimal values...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkDecimalValues(d, "GEND")
  expect_true("decimal_values" %in% names(d$tests))
  expect_true(any(grepl("In tab GEND: DECIMAL VALUES", d$info$messages$message)))
  expect_gt(nrow(d$tests$decimal_values), 0) # test the tests object
})

test_that("Can detect invalid org units...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkInvalidOrgUnits(d, "GEND")
  expect_true("invalid_orgunits" %in% names(d$tests))
  expect_true(any(grepl("In tab GEND, INVALID OR BLANK ORG UNITS", d$info$messages$message)))
  expect_gt(nrow(d$tests$invalid_orgunits), 0) # test the tests object
})

test_that("Can detect invalid or blank prioritizations...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkInvalidPrioritizations(d, "Prioritization")
  expect_true("invalid_prioritizations" %in% names(d$tests))
  expect_true(
    any(
      grepl("ERROR! In tab Prioritization: INVALID PRIORITIZATIONS",
            d$info$messages$message)))
  expect_gt(nrow(d$tests$invalid_prioritizations), 0) # test the tests object
})

test_that("Can detect altered formulas...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  d <- checkFormulas(d, "GEND")
  expect_true("altered_formulas" %in% names(d$tests))
  expect_true(any(grepl("ALTERED FORMULAS", d$info$messages$message)))
  expect_gt(nrow(d$tests$altered_formulas), 0) # test the tests object
})

test_that("Can detect defunct disaggs...", {
  d <- loadDataPack(submission_path = test_sheet("Test_COP22_Data Pack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

  expect_error(checkDisaggs(d, "PSNUxIM"), "Sorry! Can't check the PSNUxIM tab with this function.")

  d <- checkDisaggs(d, "OVC")
  expect_true("defunct_disaggs" %in% names(d$tests))
  expect_true(any(grepl("ERROR! In tab OVC: INVALID DISAGGS", d$info$messages$message)))
  expect_gt(nrow(d$tests$defunct_disaggs), 0) # test the tests object
  expect_true(d$info$has_error)
})
