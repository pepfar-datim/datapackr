context("Format vectors as set strings")

test_that("Can return NA if NA", {
  expect_identical(formatSetStrings(c(NA)), NA_character_)
})

test_that("Can return formatted set string", {
  vec <- c(1, 2, 3, 5, 7, 8, 9)
  expect_identical(formatSetStrings(vec), "1:3,5,7:9")
})

test_that("Can filter NAs in vector", {
  vec <- c(1, 2, 3, 5, NA, 7, 8, NA, 9)
  expect_identical(formatSetStrings(vec), "1:3,5,7:9")
})

test_that("Can order the set string", {
  vec <- c(9, 1, 5, 3, 2, 8, 7)
  expect_identical(formatSetStrings(vec), "1:3,5,7:9")
})

test_that("Can return NA if not a vector", {
  vec <- list(foo = "A", bar = "B")
  expect_warning(formatSetStrings(vec))
  expect_identical(supprdessWarnings(formatSetStrings(vec)), NA_character_)
})
