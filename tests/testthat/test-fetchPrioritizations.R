context("fetch-prioritizations")

with_mock_api({
  test_that("We can fetch PSNU prioritizations from DATIM", {
    prios <- fetchPrioritizationTable(data.frame(psnu_uid = c("uXwFHXCPYgj")),
                                      "2022",
                                      d2_session = training)
    print(names(prios))
    expect_true(is.data.frame(prios))
    expect_setequal(names(prios), c("psnu_uid", "prioritization", "value"))
    expect_true(is.numeric(prios$value))
    expect_true(is.character(prios$prioritization))
    expect_true(is_uidish(prios$psnu_uid))

  })
})
