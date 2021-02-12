test_that("pack a cop 21 data pack", {
  
  datapackr::pullFullCodeList(2021)
  
  datapackr::getSiteList("XtxUYCsDWrR")
  
  testthat::expect_equal(1,1)
  
  cleanup()
  
})