httptest::with_mock_api({

test_that("test getCOGSMap: ", {

data <- getCOGSMap("xRo1uG2KJHk", d2_session = datim)
data <- data[[3]]
testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 3717)
testthat::expect_named(data, c("category_option_combo",
                               "coc_uid", "category_option_group_name",
                               "category_option_group_uid" ))
rm(data) })

})
