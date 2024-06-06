context("Test memo structure")

test_that("We can warn on an invalid COP year", {
  d <- list()
  d$info$cop_year <- "1999"
  expect_warning(d2 <- memoStructure(d, d2_session = training))
  expect_null(d2$memo$structure)
  expect_identical(d, d2)
})

with_mock_api({
  test_that("We can create a memo structure", {
    d <- list()
    years <- c("2024")
    for (year in years) {
      d$info$cop_year <- year
      d <- memoStructure(d, d2_session = training)
      expect_false(is.null(d$memo$structure))
      expect_equal(typeof(d$memo$structure), "list")
      expect_setequal(names(d$memo$structure), c("row_order", "col_order", "age_order"))
      expect_true("data.frame" %in% class(d$memo$structure$row_order))
      expect_setequal(names(d$memo$structure$row_order),
                      c("ind",
                        "options",
                        "partner_chunk"))
      expect_true("data.frame" %in% class(d$memo$structure$col_order))
      expect_setequal(
        names(d$memo$structure$col_order),
        c("value",
          "name",
          "col_order",
          "id",
          "Prioritization")
      )
      expect_false(is.null(d$memo$inds))
    }

  })
})
