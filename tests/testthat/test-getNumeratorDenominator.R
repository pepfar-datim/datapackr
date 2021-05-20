context("test-numerator-denominator")


with_mock_api({
   test_that("We can get a map of numerators/denominators/data elements", {
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    n_d<-getNumeratorDenominator()
    expect_true(is.data.frame(n_d))
    expect_setequal(names(n_d),c("dataElement","numerator_denominator"))
    expect_setequal(unique(n_d$numerator_denominator),c("Numerator","Denominator"))
  })
})

with_mock_api({
  test_that("We can filter a map of numerators/denominators/data elements", {
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    n_d<-getNumeratorDenominator(dataElements = c("EtEcoXYQLW5"))
    expect_true(is.data.frame(n_d))
    expect_setequal(names(n_d),c("dataElement","numerator_denominator"))
    expect_equal(NROW(n_d),1L)
  })
})