httptest::with_mock_api({

test_that("test-pullTechArea.R: ", {

data <- getTechArea(d2_session = datim)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 2181)
testthat::expect_named(data, c("dataElement", "tech_area"))
rm(data) })

})
