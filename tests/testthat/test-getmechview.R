context("get mech view")
httptest::with_mock_api({
test_that("get mech view", {
  
datapackr::getMechanismView(d2_session)
testthat::expect_equal(1,1)

cleanup()
})
})