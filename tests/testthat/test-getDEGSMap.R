httptest::with_mock_api({

test_that("test getDEGSMap: ", {

data <- getDEGSMap("HWPJnUTMjEq", d2_session = datim)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 2195)
testthat::expect_named(data, c("name", "dataElements", "type"))
rm(data) })

})
