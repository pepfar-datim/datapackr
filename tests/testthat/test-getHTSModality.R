context("Get get HTS modalities")

with_mock_api({
  test_that("We can get HTS modalities",{
  
  foo <- getHTSModality("2021", d2_session = training)
  expect_true(any(class(foo) == "data.frame"))
  expect_setequal(names(foo), c("dataElement","hts_modality"))
  
    
  })
})

with_mock_api({
  test_that("We can error on a bad COP year",{
    
    expect_error(getHTSModality("1999", d2_session = training))

  })
})