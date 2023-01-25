context("test-loadDataPack")

test_that("Can list contents of COP22 template", {
  d  <-   datapackr::createKeychainInfo(submission_path = test_sheet("COP22_Data_Pack_Template_minimal.xlsx"),
                                        tool = "Data Pack",
                                        country_uids = NULL,
                                        cop_year = NULL)
  d <- datapackr::listWorkbookContents(d)
  expect_true(typeof(d$info$workbook_contents) == "character")
  expect_true(length(d$info$workbook_contents) > 0)
  expect_true(any(d$info$workbook_contents == "xl/workbook.xml"))

})


test_that("Can load a COP22 Data Pack.", {
  d  <-
    datapackr::loadDataPack(
      submission_path = test_sheet("COP22_Data_Pack_Template_minimal.xlsx"),
      tool = "Data Pack",
      country_uids = NULL,
      cop_year = 2022,
      load_wb = TRUE,
      load_sheets = TRUE,
      d2_session = training)

  expect_named(d, c("keychain", "info", "tool", "sheets"))
  expect_true(file.exists(d$keychain$submission_path))

  skip <- skip_tabs(tool = "Data Pack", cop_year = 2022)$unpack
  all_sheets <- unique(cop22_data_pack_schema$sheet_name)
  targets_sheets <- all_sheets[!all_sheets %in% c(skip, "PSNUxIM")]
  expect_named(d$sheets, targets_sheets)

  expect_true("wb" %in% names(d$tool))

  expect_true(d$info$tool == "Data Pack")
  expect_true(d$info$cop_year == 2022)
  expect_true(d$info$has_error == FALSE)
  expect_identical(d$info$schema, datapackr::cop22_data_pack_schema)
  expect_true(d$info$datapack_name == "Lesotho")
  expect_true(d$info$needs_psnuxim == FALSE)
  expect_true(d$info$newSNUxIM == FALSE)
  expect_true(d$info$has_psnuxim == FALSE)
  expect_true(d$info$missing_psnuxim_combos == FALSE)
  expect_true(d$info$missing_DSNUs == FALSE)
  expect_true(d$info$unallocatedIMs == FALSE)

})
