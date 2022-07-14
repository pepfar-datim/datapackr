context("test-checkStructure")

with_mock_api({
test_that("Can pass a COP21 DP Template", {
  d  <-   datapackr::createKeychainInfo(submission_path = test_sheet("COP21_Data_Pack_Template.xlsx"),
                                      tool = "Data Pack",
                                      country_uids = NULL,
                                      cop_year = NULL,
                                      d2_session = training)
  expect_silent(foo <- checkStructure(d))
  expect_equal(NROW(foo$tests$missing_sheets), 0L)
})
})

with_mock_api({
test_that("Can warn on missing COP21 DP sheet", {


  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = test_sheet("COP21_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)
  openxlsx::removeWorksheet(wb, "PMTCT")
  openxlsx::saveWorkbook(wb, file = template_copy, overwrite = TRUE)
  d <- datapackr::createKeychainInfo(submission_path = template_copy,
                                      tool = "Data Pack",
                                      country_uids = NULL,
                                      cop_year = NULL,
                                     d2_session = training)
  expect_silent(foo <- checkStructure(d))
  expect_equal(NROW(foo$tests$missing_sheets), 1L)
  expect_true(grepl("MISSING SHEETS", foo$info$messages$message))
})
})
