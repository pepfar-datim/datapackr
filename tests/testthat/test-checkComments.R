context("test-checkComments")

# test passing of template
with_mock_api({
test_that("Can pass a COP21 DP Template", {
  d  <-   datapackr::createKeychainInfo(submission_path = test_sheet("COP21_Data_Pack_Template.xlsx"),
                                         tool = "Data Pack",
                                         country_uids = NULL,
                                         cop_year = NULL,
                                        d2_session = training)
  expect_silent(foo <- checkComments(d))
  # should expect no issues so FALSE
  expect_false(foo$info$has_comments_issue)
})
})
