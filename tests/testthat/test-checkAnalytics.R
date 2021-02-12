
test_that("pack a cop 21 data pack", {
  
  d <- datapackr::unPackTool( analytics_data_path
                              , d2_session = d2_session
  )
  
  d <- datapackr::writePSNUxIM(d,
                               snuxim_model_data_path,
                               output_folder
                               , d2_session = d2_session
  )
  
  
  
  d <- datapackr::checkAnalytics(d,
                                 model_data_path
                                 ,d2_session = d2_session
  )
  testthat::expect_equal(1,1)
  
  cleanup()
  
})