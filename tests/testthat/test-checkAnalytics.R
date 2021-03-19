context("check analytics")
httptest::with_mock_api({
test_that("check analytics", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
  d <- datapackr::unPackTool( analytics_data_path
                              , d2_session = d2_session
  )
  
  suppressWarnings(d <- datapackr::writePSNUxIM(d,
                               snuxim_model_data_path,
                               output_folder
                               , d2_session = d2_session
  ))
  
  d <- datapackr::checkAnalytics(d,
                                 model_data_path
                                 ,d2_session = d2_session
  )
  testthat::expect_equal(1,1)
  
  cleanup()
  
})
})