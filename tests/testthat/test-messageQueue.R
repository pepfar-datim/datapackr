test_that("Can error if messages and levels are not equal length", {

  foo <- MessageQueue()
  messages <- c("Hello", "Hi there")
  level <- c("INFO", "WARNING", "ERROR")

  foo <- expect_error(appendMesage(foo, messages, level)) })

test_that("Can error on a NA message", {

  foo <- MessageQueue()
  messages <- c("Hello", NA_character_)
  level <- c("INFO", "WARNING")

  foo <- expect_error(appendMesage(foo, messages, level)) })


test_that("Can error on a NA level", {

  foo <- MessageQueue()
  messages <- c("Hello", "Goodbye")
  level <- c("INFO", NA)

  foo <- expect_error(appendMesage(foo, messages, level)) })
