context("get mech view")
httptest::with_mock_api({
test_that("get mech view", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
datapackr::getMechanismView(d2_session)
testthat::expect_equal(1,1)

cleanup()
})
})