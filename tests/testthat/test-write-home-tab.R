context("test-write-home-tab")

test_that("Can write a home tab", {
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet('COP21_Data_Pack_Template.xlsx'), to=template_copy)
  wb<-openxlsx::loadWorkbook(template_copy)
  openxlsx::removeWorksheet(wb,"Home")
  datapackr::writeHomeTab(wb,datapack_name = "Lesotho",country_uids = "qllxzIjjurr",cop_year = 2021)
  openxlsx::saveWorkbook(wb,file=template_copy,overwrite = TRUE)
  d<-datapackr::createKeychainInfo(template_copy)
  testthat::expect_setequal(names(d), c("info", "keychain"))
  testthat::expect_setequal(
    names(d$info),
    c(
      "datapack_name",
      "tool",
      "country_uids",
      "cop_year",
      "schema",
      "has_error",
      "newSNUxIM",
      "has_psnuxim",
      "missing_psnuxim_combos",
      "missing_DSNUs",
      "needs_psnuxim"
    )
  )
  expect_equal(d$keychain$submission_path,template_copy)
  expect_null(d$info$warning_msg)
  expect_false(d$info$has_error)
  expect_false(d$info$newSNUxIM)
  expect_equal(d$info$country_uids,"qllxzIjjurr")
  expect_equal(d$info$datapack_name,"Lesotho")
  expect_false(d$info$newSNUxIM)
  expect_false(d$info$has_error)
  expect_false(d$info$missing_DSNUs)
  expect_false(d$info$missing_psnuxim_combos)
  expect_equal(d$info$tool, "Data Pack")
  expect_equal(d$info$cop_year, 2021)
  expect_false(d$info$needs_psnuxim)
  unlink(template_copy)
} )