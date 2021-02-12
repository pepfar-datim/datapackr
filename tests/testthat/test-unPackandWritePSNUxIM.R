test_that("unpack and write PSNUxIM", {
 
   d <- datapackr::unPackTool("~/datapackr_test_files/Testing/No PSNUxIM/Data Pack_Zambia_20210121180718.xlsx"
                             ,d2_session = d2_session
  )
  
  datapackr::writePSNUxIM(d,
                          snuxim_model_data_path,
                          output_folder
                          , d2_session = d2_session
  )  
  
  testthat::expect_equal(1,1)
  cleanup()
})