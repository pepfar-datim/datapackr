context("can-check missing meta data...")

with_mock_api({
  test_that("Can check missing meta data in all sheets", {
    
    d <- datapackr::createKeychainInfo(
      submission_path = test_sheet("COP21_DP_random_no_psnuxim.xlsx"),
      tool = "Data Pack",
      country_uids = NULL,
      cop_year = NULL,
      d2_session = NULL
    )
    
    # check no errors pop for no missing meta data aka false positive
    sheets <- sheets <- grep("PSNUxIM", names(d$sheets), value = TRUE, invert = TRUE)
    for (sheets in sheets) {
      d <- checkMissingMetadata(d, sheet)
    }
    
    testthat::expect_identical(d$info$messages$message, character(0))
    
    # create missing metadata and and test if it gets caught
    
    
    
    
  })
})
