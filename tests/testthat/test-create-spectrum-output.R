test_that("create_play_spectrum_output", {
  
  datapackr::create_play_spectrum_output("XtxUYCsDWrR",
                                         2020,
                                         "~/datapackr_test_files"
                                         , d2_session = d2_session)
  
  testthat::expect_equal(1,1)
  cleanup()
  
})