context("test-signature")


test_that("Can generate a key chain from a Data Pack Template", {
  d <- createKeychainInfo(submission_path = getTemplate("COP23_Data_Pack_Template.xlsx"),
                        tool = "Data Pack",
                        country_uids = NULL,
                        cop_year = NULL,
                        d2_session = NULL)
  testthat::expect_setequal(names(d), c("info", "keychain"))
  testthat::expect_setequal(
     names(d$info),
     c(
       "datapack_name",
       "sane_name",
       "tool",
       "country_uids",
       "cop_year",
       "operating_unit",
       "schema",
       "has_error",
       "newSNUxIM",
       "has_psnuxim",
       "missing_psnuxim_combos",
       "missing_DSNUs",
       "needs_psnuxim",
       "unallocatedIMs",
       "messages"
     )
   )
   expect_equal(d$keychain$submission_path, getTemplate("COP23_Data_Pack_Template.xlsx"))
   expect_setequal(class(d$info$messages), c("MessageQueue"))
   expect_false(d$info$has_error)
   expect_false(d$info$newSNUxIM)
   expect_equal(d$info$country_uids, "qllxzIjjurr")
   expect_equal(d$info$datapack_name, "Lesotho")
   expect_equal(d$info$sane_name, "Lesotho")
   expect_equal(d$info$operating_unit, tibble::tibble(ou = "Lesotho",
                                                      ou_uid = "qllxzIjjurr"))
   expect_null(d$info$source_user)
   expect_false(d$info$newSNUxIM)
   expect_false(d$info$has_error)
   expect_false(d$info$missing_DSNUs)
   expect_false(d$info$missing_psnuxim_combos)
   expect_false(d$info$unallocatedIMs)
   expect_equal(d$info$tool, "Data Pack Template")
   expect_equal(d$info$cop_year, 2023)
   expect_false(d$info$needs_psnuxim)
})

test_that("Can get the type and COP year of tool of a COP22 Data Pack", {

   d <- datapackr::createKeychainInfo(submission_path = test_sheet("COP22_DataPack_unPackingChecks.xlsx"))
   expect_equal(d$info$tool, "Data Pack")
   expect_equal(d$info$cop_year, 2022)
})
