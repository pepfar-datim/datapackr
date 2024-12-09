context("Utility functions")

test_that("can properly round numbers", {
  expect_equal(round_trunc(0), 0L)
  expect_equal(round_trunc(0.4), 0L)
  expect_equal(round_trunc(0.499), 0L)
  expect_equal(round_trunc(0.5), 1L)
  expect_equal(round_trunc(-0.499), 0L)
  expect_equal(round_trunc(-0.5), -1L)
  expect_equal(round_trunc(-2.5), -3L)
  expect_equal(round_trunc(-2.4231), -2L)
  expect_equal(round_trunc(100), 100L) })

test_that("Can get current COP year", {
  expect_true(is.numeric(getCurrentCOPYear()))
  expect_equal(getCurrentCOPYear(), 2025)
})
