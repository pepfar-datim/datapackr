context("Test memo structure")

test_that("We can warn on an invalid COP year", {
  d <- list()
  d$info$cop_year <- "1999"
  expect_warning(d2 <- memo_structure(d, d2_session = training))
  expect_null(d2$memo$structure)
  expect_identical(d, d2)
})

with_mock_api({
test_that("We can create a memo structure", {
  d <- list()
  d$info$cop_year <- "2022"
  datimutils::loginToDATIM(config_path = test_config("test-config.json"))
  expect_silent(d <- memo_structure(d, d2_session = d2_default_session))
  expect_false(is.null(d$memo$structure))
  expect_equal(typeof(d$memo$structure), "list")
  expect_setequal(names(d$memo$structure), c("row_order", "col_order"))
  expect_true("data.frame" %in% class(d$memo$structure$row_order))
  expect_setequal(names(d$memo$structure$row_order), c("ind",
                                                       "options",
                                                       "partner_chunk"))
  expect_true("data.frame" %in% class(d$memo$structure$col_order))
  expect_setequal(names(d$memo$structure$col_order), c("value",
                                                       "name",
                                                       "col_order",
                                                       "id",
                                                       "Prioritization"))
  expect_false(is.null(d$memo$inds))
  
  }) })

