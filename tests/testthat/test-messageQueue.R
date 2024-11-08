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
  expect_equal(foo$message, "INFO: Hello")
  expect_equal(foo$level, "INFO")

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

test_that("Can error on if messages are not a vector", {

  foo <- MessageQueue()
  message <- c(NA, NULL, "")
  level <- c(NULL, "", NA)
  messages_df <- data.frame(message = message, level = level)
  expect_error(appendMessage(foo, messages_df, level))

  messages_list <- list(message = message, level = level)
  expect_error(appendMessage(foo, messages_list, level))

  messages_list <- list(message = message, level = list(level = level))
  expect_error(appendMessage(foo, messages_list, level))

  messages_list <- list(message = message, level = level, tool = list(tool = "DataPack"))
  expect_error(appendMessage(foo, messages_list, level))

  foo <- MessageQueue()
  message <- c("a", "b", "c")
  level <- c("ERROR", "ERROR", "ERROR", "WARNING")
  expect_error(appendMessage(foo, message, level), "Messages and warnings must be of the same length")

})

test_that("Can warn on an empty message or warning", {

  foo <- MessageQueue()
  message <- c("INFO: Hello", "")
  level <- c("INFO", "WARNING")

  expect_warning(foo <- appendMessage(foo, message, level))
  expect_equal(foo$message, "INFO: Hello")
  expect_equal(foo$level, "INFO")

  foo <- MessageQueue()
  message <- c("INFO: Hello", "WARNING: Goodbye")
  level <- c("INFO", "")

  expect_warning(foo <- appendMessage(foo, message, level))
  expect_setequal(foo$message, message)
  expect_setequal(foo$level, c("INFO", "UNKNOWN"))

})

test_that("Can add a tool (optional)", {

  foo <- MessageQueue()
  message <- c("INFO: Hello", "WARNING: Goodbye")
  level <- c("INFO", "WARNING")
  tool <- c("Tool1", NA_character_)

  foo <- appendMessage(foo, message, level, tool)
  expect_setequal(names(foo), c("message", "level", "tool"))
  expect_equal(foo$tool, c("Tool1", "UNKNOWN"))
})

test_that("printMessages.MessageQueue works as expected", {
  mockery::stub(printMessages.MessageQueue, "interactive", TRUE)
  foo <- MessageQueue()
  message <- c("INFO: Hello", "WARNING: Goodbye")
  level <- c("INFO", "WARNING")
  tool <- c("Tool1", NA_character_)
  foo <- appendMessage(foo, message, level, tool)
  output <- capture.output(printMessages.MessageQueue(foo))
  expect_true(any(grepl("VALIDATION ISSUES:", output)))
  expect_true(any(grepl("WARNING!: Problematic", output)))
})
