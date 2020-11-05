httptest::with_mock_api({

test_that("test getHTSModality: ", {

data <- getHTSModality(cop_year = 2020)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 88)
testthat::expect_named(data, c("dataElement", "hts_modality"))
rm(data) })

})
