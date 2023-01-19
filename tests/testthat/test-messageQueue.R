test_that("Can error if messages and levels are not equal length", {

  foo <- MessageQueue()
  message <- c("Hello", "Hi there")
  level <- c("INFO", "WARNING", "ERROR")

  expect_error(appendMessage(foo, message, level)) })

test_that("Can error on a NA message with a level", {

  foo <- MessageQueue()
  message <- c("INFO: Hello", NA_character_)
  level <- c("INFO", "WARNING")

  expect_warning(foo <- appendMessage(foo, message, level))
  expect_equal("INFO: Hello", foo$message)
  expect_equal("INFO", foo$level)

  })


test_that("Can proceed without a level if there is a message", {

  foo <- MessageQueue()
  message <- c("INFO: Hello", "Goodbye")
  level <- c("INFO", NA)

  expect_warning(foo <- appendMessage(foo, message, level))
  expect_setequal(message, foo$message)
  expect_setequal(c("INFO", "UNKNOWN"), foo$level)
  })

test_that("Can append a simple message", {

  foo <- MessageQueue()
  message <- c("INFO: Hello")
  level <- c("INFO")
  foo <- appendMessage(foo, message, level)
  expect_equal(message, foo$message)
  expect_equal(level, foo$level)

}
)

test_that("Can do nothing if message and level are empty", {

  foo <- MessageQueue()
  message <- c(NA, NULL, "")
  level <- c(NULL, "", NA)
  bar <- appendMessage(foo, message, level)
  expect_identical(foo, bar)

}
)
