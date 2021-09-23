context("test-get-datapack-name")

test_that("Can read a Datapack Name and UIDs", {
  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = test_sheet("COP21_Data_Pack_Template.xlsx"), to = template_copy)
  foo <- unPackDataPackName(submission_path = template_copy,
                          "Data Pack")
  expect_equal(foo, "Lesotho")
  foo <- unPackCountryUIDs(submission_path = template_copy,
                         tool = "Data Pack")
  expect_equal(foo, "qllxzIjjurr")
  unlink(template_copy)
})

test_that("Can error on an invalid regional DataPack UID", {
  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = test_sheet("COP21_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)

  home_address <- cellranger::as.cell_addr(countryUIDs_homeCell(), strict = FALSE)
  openxlsx::writeData(wb = wb, sheet = "Home",
                      x = "Asia_Regional_Data_Pack",
                      xy = c(home_address$col, home_address$row))
  openxlsx::saveWorkbook(wb = wb, file = template_copy, overwrite = TRUE)
  expect_warning(unPackCountryUIDs(submission_path = template_copy,
                                               tool = "Data Pack",
                                               cop_year = 2021))
  unlink(template_copy)
})
