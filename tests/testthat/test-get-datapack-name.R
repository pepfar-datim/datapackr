context("test-get-datapack-name")

test_that("Can read a Datapack Name and UIDs", {
  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = getTemplate("COP23_Data_Pack_Template.xlsx"), to = template_copy)
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
  file.copy(from = getTemplate("COP23_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)

  home_address <- cellranger::as.cell_addr(countryUIDs_homeCell(), strict = FALSE)
  openxlsx::writeData(wb = wb, sheet = "Home",
                      x = "Asia_Regional_Data_Pack",
                      xy = c(home_address$col, home_address$row))
  openxlsx::saveWorkbook(wb = wb, file = template_copy, overwrite = TRUE)
  expect_error(unPackCountryUIDs(submission_path = template_copy,
                                               tool = "Data Pack",
                                               cop_year = 2023))
  unlink(template_copy)
})

# Wed May 29 15:30:08 2024 --- We no longer support "OPU Data Pack"
# test_that("Can read a COP22 OPU DataPack name and country_uid", {
#   template_copy <- paste0(tempfile(), ".xlsx")
#   file.copy(from = getTemplate("COP23_OPU_Data_Pack_Template.xlsx"), to = template_copy)
#
#   foo <- unPackDataPackName(submission_path = template_copy,
#                           "OPU Data Pack")
#   expect_equal(foo, "Lesotho")
#   #We expect a warning here because the PSNUxIM tab has no PSNUs
#   foo <- suppressWarnings(unPackCountryUIDs(submission_path = template_copy,
#                          tool = "OPU Data Pack"))
#   expect_equal(foo, "qllxzIjjurr")
#   unlink(template_copy)
# })

test_that("Can parse valid PSNU UIDs for COP23 OPU Datapack", {
  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = getTemplate("COP23_OPU_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)

  openxlsx::writeData(wb = wb, sheet = "PSNUxIM",
                      x = "Lesotho > Berea [#SNU] [wpg5evyl1OL]",
                      startCol = 1,
                      startRow = 15)

  openxlsx::saveWorkbook(wb = wb, file = template_copy, overwrite = TRUE)
  foo <- parsePSNUs(template_copy, "OPU Data Pack", "2023")
  expect_equal(typeof(foo), "list")
  expect_setequal(names(foo), c("PSNU", "psnu_uid", "country_name", "country_uid"))
  expect_equal(foo$PSNU, "Lesotho > Berea [#SNU] [wpg5evyl1OL]")
  expect_equal(foo$psnu_uid, "wpg5evyl1OL")
  expect_equal(foo$country_name, "Lesotho")
  expect_equal(foo$country_uid, "qllxzIjjurr")
  unlink(template_copy)
})


test_that("Can error on  invlid PSNU UIDs for COP23 OPU Datapack", {
  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = getTemplate("COP23_OPU_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)

  openxlsx::writeData(wb = wb, sheet = "PSNUxIM",
                      x = "Bogus PSNU",
                      startCol = 1,
                      startRow = 15)

  openxlsx::saveWorkbook(wb = wb, file = template_copy, overwrite = TRUE)
  expect_error(parsePSNUs(template_copy, "OPU Data Pack", "2023"))

  unlink(template_copy)
})
