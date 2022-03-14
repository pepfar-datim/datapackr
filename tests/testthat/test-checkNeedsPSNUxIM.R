context("test-checkHasPSNUxIM")

test_that("Can error if d$data$PSNUxIM is not a data frame", {
  
  d <- list()
  expect_error(checkHasPSNUxIM(d))

})

test_that("Can check empty PSNUxIM", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$data$SNUxIM <- data.frame(foo = c(NA), bar =c(NA))
  d$info$tool <- "Data Pack"
  d <- checkHasPSNUxIM(d)
  expect_false(d$info$has_psnuxim)
  expect_true(d$info$needs_psnuxim)
  expect_true(stringr::str_detect(d$info$messages$message,"Your Data Pack needs a new PSNUxIM tab."))
})

test_that("Can check PSNUxIM with data exists", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$data$SNUxIM <- data.frame(foo = c(1,2), bar =c(3,4))
  d$info$tool <- "Data Pack"
  d <- checkHasPSNUxIM(d)
  expect_true(d$info$has_psnuxim)
  expect_true(is.null(d$info$needs_psnuxim))

})