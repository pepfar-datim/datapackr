test_that("pack a cop 21 data pack", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_default_session")
  
  datapackr::pullFullCodeList(2021)
  
  datapackr::getSiteList("XtxUYCsDWrR")
  
  testthat::expect_equal(1,1)
  
  cleanup()
  
})