httptest::with_mock_api({

test_that("test getNumeratorDenominator: ", {

data <- getNumeratorDenominator(d2_session = datim)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 2103)
testthat::expect_named(data, c("dataElement", "numerator_denominator"))
rm(data) })

})
