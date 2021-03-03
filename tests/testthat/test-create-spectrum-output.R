context("create_play_spectrum_output")
httptest::with_mock_api({
test_that("create_play_spectrum_output", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
  datapackr::create_play_spectrum_output("XtxUYCsDWrR",
                                         2020,
                                         test_directory
                                         , d2_session = d2_session)
  
  testthat::expect_equal(1,1)
  cleanup()
  
})
})

  