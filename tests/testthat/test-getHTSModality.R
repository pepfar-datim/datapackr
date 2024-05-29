context("Get get HTS modalities")

with_mock_api({
  test_that("We can get HTS modalities", {

  foo <- getHTSModality("2024", d2_session = training)
  expect_true(any(class(foo) == "data.frame"))
  expect_setequal(names(foo), c("dataElement", "hts_modality"))
  expect_true(all(is_uidish(foo$dataElement)))

  })
})

with_mock_api({
  test_that("We can error on a bad COP year", {

    expect_error(getHTSModality("1999", d2_session = training))

  })
})
