context("Memo generation tests")


with_mock_api({
  test_that("We can  issue a warning if the COP year is not found", {
    expect_warning(inds <- datapackr::getMemoIndicators("1999", d2_session = training))
    expect_null(inds)
  })
})


with_mock_api({
  test_that("We can get a data frame of memo indicators", {
    inds <- datapackr::getMemoIndicators("2022", d2_session = training)
    expect_type(inds,"list")
    expect_setequal(names(inds),c("id","name","numerator","denominator"))
  })
})