context("test-unPackingChecks")

test_that("Can detect invalid comment types ...", {

  #Note: A warning is thrown here on the command line  for "invalid parameter"
  # Does not have any impact on the parsing, but documenting it nonetheless.
  expect_warning(d <- loadDataPack(submission_path = test_sheet("COP22_DataPack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = TRUE,
                    load_sheets = TRUE,
                    d2_session = training))

  d <- checkToolComments(d)

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
  expect_warning(d <- loadDataPack(submission_path = test_sheet("COP22_DataPack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = TRUE,
                    load_sheets = FALSE,
                    d2_session = training))

  d <- checkToolConnections(d)

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


test_that("Can check sheet data...", {
  d <- loadDataPack(submission_path = test_sheet("COP22_DataPack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = FALSE,
                    load_sheets = TRUE,
                    d2_session = training)

   d <- checkSheetData(d)

  expect_true("duplicate_rows" %in% names(d$tests))
  expect_true("missing_cols" %in% names(d$tests))
  expect_true("duplicate_columns" %in% names(d$tests))
  expect_true("columns_out_of_order" %in% names(d$tests))
  expect_true("non_numeric" %in% names(d$tests))
  expect_true("negative_values" %in% names(d$tests))
  expect_true("decimal_values" %in% names(d$tests))
  expect_true("invalid_orgunits" %in% names(d$tests))
  expect_true("invalid_prioritizations" %in% names(d$tests))
  expect_true("altered_formulas" %in% names(d$tests))
  expect_true("defunct_disaggs" %in% names(d$tests))

  expect_true(any(grepl("In tab GEND: DUPLICATE ROWS", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND, MISSING COLUMNS", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND, DUPLICATE COLUMNS", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND, OUT OF ORDER COLUMNS", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND: NON-NUMERIC VALUES", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND: NEGATIVE VALUES", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND: DECIMAL VALUES", d$info$messages$message)))
  expect_true(any(grepl("In tab GEND, INVALID OR BLANK ORG UNITS", d$info$messages$message)))
  expect_true(any(grepl("ERROR! In tab Prioritization: INVALID PRIORITIZATIONS", d$info$messages$message)))
  expect_true(any(grepl("ALTERED FORMULAS", d$info$messages$message)))
  expect_true(any(grepl("ERROR! In tab OVC: INVALID DISAGGS", d$info$messages$message)))

  expect_equal(nrow(d$tests$duplicate_rows), 4L)
  expect_equal(nrow(d$tests$missing_cols), 1L)
  expect_equal(nrow(d$tests$duplicate_columns), 2L)
  expect_equal(nrow(d$tests$columns_out_of_order), 2L)
  expect_equal(nrow(d$tests$non_numeric), 819L)
  expect_equal(nrow(d$tests$negative_values), 6L)
  expect_equal(nrow(d$tests$decimal_values), 29L)
  expect_equal(nrow(d$tests$invalid_orgunits), 2L)
  expect_equal(nrow(d$tests$invalid_prioritizations), 4L)
  expect_equal(nrow(d$tests$altered_formulas), 22L)
  expect_equal(nrow(d$tests$defunct_disaggs), 12L)

  expect_true(d$info$has_error)
})


test_that("Can check index header columns exist", {

  d <- list()
  d$info$schema <- datapackr::cop22_data_pack_schema
  d$info$messages <- MessageQueue()

  #Get the HTS sheet headers. This one should pass

  hts_col_headers <- d$info$schema %>%
    dplyr::filter(sheet_name == "HTS",
                  col_type == "row_header",
                  !indicator_code %in% c("SNU1", "ID")) %>%
    dplyr::pull(indicator_code)

  mock_data <- data.frame(t(rep("abc123", length(hts_col_headers))))
  hts_df <- mock_data
  names(hts_df) <- hts_col_headers

  d$sheets$HTS <- hts_df

  #Fake another one

  bogus_sheet <- mock_data

  d$sheets$KP_MAT <- bogus_sheet

  d <- checkToolEmptySheets(d, sheets = c("HTS", "KP_MAT"))
  expect_true(is.data.frame(d$tests$missing_index_columns))
  expect_setequal(names(d$tests$missing_index_columns), c("sheet_name"))
  expect_equal(d$tests$missing_index_columns$sheet_name, "KP_MAT")

})
