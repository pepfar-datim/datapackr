context("Test the build")

test_that("We can fail the build", {
  skip("Skip failing the build unless we need to test it.")
  expect_equal(2, 1) #nolint yoda_test_linter
})
