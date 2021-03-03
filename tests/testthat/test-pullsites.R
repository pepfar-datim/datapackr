context("pull sites")
httptest::with_mock_api({
test_that("pull sites", {
  
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_default_session")
  
  
  datapackr::getSiteList("XtxUYCsDWrR")
  
  testthat::expect_equal(1,1)
  
  cleanup()
  
})
})