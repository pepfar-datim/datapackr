context("unpack and write PSNUxIM")
httptest::with_mock_api({
test_that("unpack and write PSNUxIM", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
  print(zambia_path)
 
   d <- datapackr::unPackTool(zambia_path,
                             d2_session = d2_session
  )
  
  datapackr::writePSNUxIM(d,
                          snuxim_model_data_path,
                          output_folder
                          , d2_session = d2_session
  )  
  
  testthat::expect_equal(1,1)
  cleanup()
})
})