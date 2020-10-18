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
       "has_error",
       "newSNUxIM",
       "has_psnuxim",
       "missing_psnuxim_combos"
     )
   )
   expect_null(d$info$warning_msg)
   expect_false(d$info$has_error)
   expect_false(d$info$newSNUxIM)
   #expect_equal(d$info$cop_year,getCurrentCOPYear())
   #Since we are using a specific version of the template for these tests
   expect_equal(d$info$cop_year,2020)
  
} )

