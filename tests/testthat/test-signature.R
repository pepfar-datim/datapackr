context("test-signature")

test_that("Can generate a signature", {
   
   #demostration of why using openxlsx will not work, tests will fail, end file is corrupted
   # wb <- openxlsx::loadWorkbook("sheets/COP20_Data_Pack_Template_vFINAL.xlsx")
   # df <- data.frame("a" = "e5s0nMiYRt2","b" =1, "c" = 1,"d" = 1,"e" = 1,"f" = 1,"g" = 1,"h" = 1,"i" = 1,"j" = 1,"k" = 1,"l" = '1')
   # openxlsx::writeData(wb, sheet = "Prioritization", x = df,  startRow = 15, colNames = F, rowNames = F)
   # df <- data.frame("a" = "IH1kchw86uA")
   # openxlsx::writeData(wb, sheet = "Home", x = df,  startRow = 25, startCol = 2, colNames = F, rowNames = F)
   # openxlsx::saveWorkbook(wb, "sheets/COP20_Data_Pack_Template_v2.xlsx", overwrite = T)
   # 
   
   # the correct way using xlsx
   # wb <- xlsx::loadWorkbook("sheets/COP20_Data_Pack_Template_vFINAL.xlsx")
   # df <- data.frame("a" = "Chiradzuluu District [#SNU] [e5s0nMiYRt2]","b" =1, "c" = 1,"d" = 1,"e" = 1,"f" = 1,"g" = 1,"h" = 1,"i" = 1,"j" = 1,"k" = 1,"l" = '1')
   # xlsx::addDataFrame(df, xlsx::getSheets(wb)$Prioritization, startRow = 15, col.names = F, row.names = F)
   # df <- data.frame("a" = "IH1kchw86uA")
   # xlsx::addDataFrame(df, xlsx::getSheets(wb)$Home, startRow = 25, startColumn = 2, col.names = F, row.names = F)
   # xlsx::saveWorkbook(wb, "sheets/COP20_Data_Pack_Template_v2.xlsx")
   
   d <- datapackr:::createKeychainInfo(submission_path = 'sheets/COP20_Data_Pack_Template_v2.xlsx',
                        tool = "Data Pack",
                        country_uids = NULL,
                        cop_year = NULL) 
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
       "missing_DSNUs"
     )
   )
   expect_null(d$info$warning_msg)
   expect_false(d$info$has_error)
   expect_false(d$info$newSNUxIM)

  
} )

test_that("Can get the type and COP year of tool of a COP20 Data Pack",{

   d <- datapackr:::createKeychainInfo(submission_path = test_sheet('COP20_Data_Pack_Template_v2.xlsx'))
   expect_equal(d$info$tool, "Data Pack")
   expect_equal(d$info$cop_year, 2020)
})
