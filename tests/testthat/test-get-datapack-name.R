context("test-get-datapack-name")


test_that("Can read a Datapack Name and UIDs", {
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet('COP20_Data_Pack_Template_vFINAL.xlsx'), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  home_address<-cellranger::as.cell_addr(datapackr:::dataPackName_homeCell(), strict = FALSE)
  openxlsx::writeData(wb = wb,sheet="Home", 
                      x="Demoland",
                      xy = c(home_address$col,home_address$row))
  
  home_address<-cellranger::as.cell_addr(datapackr:::countryUIDs_homeCell(), strict = FALSE)
  openxlsx::writeData(wb = wb,sheet="Home", 
                      x="abc12345678",
                      xy = c(home_address$col,home_address$row))
  
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  foo<-datapackr:::unPackDataPackName(submission_path = template_copy,
                          "Data Pack Template")
  expect_equal(foo,"Demoland")
  foo<-datapackr:::unPackCountryUIDs(submission_path = test_sheet("COP20_Data_Pack_Template_v2.xlsx"),
                         tool = "Data Pack")
  expect_equal(foo,"IH1kchw86uA")
  unlink(template_copy)
} )

test_that("Can error on an invalid regional DataPack UID", {
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet('COP20_Data_Pack_Template_vFINAL.xlsx'), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  
  home_address<-cellranger::as.cell_addr(datapackr:::countryUIDs_homeCell(), strict = FALSE)
  openxlsx::writeData(wb = wb,sheet="Home", 
                      x="Asia_Regional_Data_Pack",
                      xy = c(home_address$col,home_address$row))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  expect_error(datapackr:::unPackCountryUIDs(submission_path = template_copy))
  unlink(template_copy)
} )



