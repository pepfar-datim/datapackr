context("test-signature")


test_that("Can generate a signature", {
  d <- datapackr:::createKeychainInfo(submission_path = test_sheet('COP20_Data_Pack_Template_vFINAL.xlsx'),
                        tool = "Data Pack Template",
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
   
   d <- datapackr:::createKeychainInfo(submission_path = test_sheet('COP20_Data_Pack_Template_vFINAL.xlsx'))
   expect_equal(d$info$tool, "Data Pack Template")
   expect_equal(d$info$cop_year, 2020)
})
