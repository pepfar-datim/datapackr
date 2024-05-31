# Define the test
test_that("strip_wb_NAs removes NA sharedStrings correctly", {

    # mock workbook object
    d <- list(
      tool = list(
        wb = list(
          sharedStrings = c("<si><t>NA</t></si>", "<si><t>Some text</t></si>"),
          worksheets = list(
            list(sheet_data = list(v = c(0, 1, 0), t = c(1, 1, 1))),
            list(sheet_data = list(v = c(1, 0, 1), t = c(1, 1, 1)))
          )
        )
      )
    )

    # apply function
    result <- strip_wb_NAs(d)

    # check that sharedStrings are unchanged
    expect_equal(result$tool$wb$sharedStrings, c("<si><t>NA</t></si>", "<si><t>Some text</t></si>"))

    # check worksheet data is modified correctly for the first worksheet
    expect_equal(result$tool$wb$worksheets[[1]]$sheet_data$v, c(NA_character_, 1, NA_character_))
    expect_equal(result$tool$wb$worksheets[[1]]$sheet_data$t, c(NA, 1, NA))

    # Check worksheet data is modified correctly for the second worksheet
    expect_equal(result$tool$wb$worksheets[[2]]$sheet_data$v, c(1, NA_character_, 1))
    expect_equal(result$tool$wb$worksheets[[2]]$sheet_data$t, c(1, NA, 1))
})
