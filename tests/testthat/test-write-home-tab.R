context("Write a DataPack Home tab")

test_that("Can write a home tab", {
  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = getTemplate("COP23_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)
  openxlsx::removeWorksheet(wb, "Home")
  datapackr::writeHomeTab(wb, datapack_name = "Lesotho", country_uids = "qllxzIjjurr", cop_year = 2023)
  openxlsx::saveWorkbook(wb, file = template_copy, overwrite = TRUE)
  d <- datapackr::createKeychainInfo(template_copy)
  testthat::expect_setequal(names(d), c("info", "keychain"))
  testthat::expect_setequal(
    names(d$info),
    c(
      "tool",
      "country_uids",
      "cop_year",
      "messages",
      "has_error",
      "schema",
      "datapack_name",
      "sane_name",
      "operating_unit",
      "needs_psnuxim",
      "newSNUxIM",
      "has_psnuxim",
      "missing_psnuxim_combos",
      "missing_DSNUs",
      "unallocatedIMs"
    )
  )
  expect_equal(d$keychain$submission_path, template_copy)
  expect_setequal(class(d$info$messages), c("MessageQueue"))
  expect_equal(length(d$info$messages$message), 0L)
  expect_false(d$info$has_error)
  expect_false(d$info$newSNUxIM)
  expect_equal(d$info$country_uids, "qllxzIjjurr")
  expect_equal(d$info$datapack_name, "Lesotho")
  expect_equal(d$info$sane_name, "Lesotho")
  expect_equal(d$info$operating_unit, tibble::tibble(ou = "Lesotho",
                                                     ou_uid = "qllxzIjjurr"))
  expect_false(d$info$newSNUxIM)
  expect_false(d$info$has_error)
  expect_false(d$info$missing_DSNUs)
  expect_false(d$info$missing_psnuxim_combos)
  expect_false(d$info$unallocatedIMs)
  expect_equal(d$info$tool, "Data Pack Template")
  expect_equal(d$info$cop_year, 2023)
  expect_false(d$info$needs_psnuxim)
  unlink(template_copy)
})
