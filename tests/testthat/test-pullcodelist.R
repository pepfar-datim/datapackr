context("pull code list")
httptest::with_mock_api({
test_that("pull code list", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_default_session")
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
  datapackr::pullFullCodeList(2021)
   
  testthat::expect_equal(1,1)
  
  cleanup()
  
})
})


