context("test-extractWorkbook")

test_that("Can list contents of COP22 template", {
  d  <-   datapackr::createKeychainInfo(submission_path = test_sheet("COP22_Data_Pack_Template_minimal.xlsx"),
                                        tool = "Data Pack",
                                        country_uids = NULL,
                                        cop_year = NULL)
  d <- datapackr:::listWorkbookContents(d)
  expect_true(typeof(d$info$worbook_contents) == "character")
  expect_true(length(d$info$worbook_contents) > 0)
  expect_true(any(d$info$worbook_contents == "xl/workbook.xml"))

})


test_that("Can extract contents of a COP22 template", {
  d  <-   datapackr::createKeychainInfo(submission_path = test_sheet("COP22_Data_Pack_Template_minimal.xlsx"),
                                        tool = "Data Pack",
                                        country_uids = NULL,
                                        cop_year = NULL)
  d <- datapackr:::extractWorkbook(d)
  expect_true(d$info$has_extract)
  expect_true(dir.exists(d$keychain$extract_path))
  expect_true(any(list.files(d$keychain$extract_path) == "xl"))
  unlink(d$keychain$extract_path, recursive = TRUE)
})
