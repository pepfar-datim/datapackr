context("fetch-prioritizations")

with_mock_api({
  test_that("We can fetch PSNU prioritizations from DATIM", {
    prios <- fetchPrioritizationTable(c("uXwFHXCPYgj"),
                                      "2022",
                                      d2_session = training)
    expect_true(is.data.frame(prios))
    expect_setequal(names(prios), c("orgUnit", "value"))
    expect_true(all(is.numeric(prios$value)))
    expect_true(all(is_uidish(prios$orgUnit)))

  })
})
