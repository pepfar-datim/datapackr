context("test-numerator-denominator")


with_mock_api({
   test_that("We can get a map of numerators/denominators/data elements", {
    n_d <- getNumeratorDenominator(d2_session = training)
    expect_true(is.data.frame(n_d))
    expect_setequal(names(n_d),c("dataElement","numerator_denominator"))
    expect_setequal(unique(n_d$numerator_denominator),c("Numerator","Denominator"))
  })
})

with_mock_api({
  test_that("We can filter a map of numerators/denominators/data elements", {
    n_d <- getNumeratorDenominator(dataElements = c("EtEcoXYQLW5"),
                                 d2_session = training)
    expect_true(is.data.frame(n_d))
    expect_setequal(names(n_d),c("dataElement","numerator_denominator"))
    expect_equal(NROW(n_d),1L)
  })
})
