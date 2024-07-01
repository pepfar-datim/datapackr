context("Memo generation tests")


with_mock_api({
  test_that("We can  issue a warning if the COP year is not found", {
    expect_warning(inds <- datapackr::getMemoIndicators("1999", d2_session = training))
    expect_null(inds)
  })
})


with_mock_api({
  test_that("We can get a data frame of memo indicators", {
    inds <- datapackr::getMemoIndicators("2024", d2_session = training)
    expect_type(inds, "list")
    expect_setequal(names(inds), c("id", "name", "numerator", "denominator"))
  })
})


with_mock_api({
  test_that("We can fetch existing prioritizations", {
    cop_year <- 2024
    psnus <- getValidOrgUnits(cop_year) %>% dplyr::filter(country_name == "Lesotho") %>% dplyr::pull(uid)
    prio_table <- fetchPrioritizationTable(psnus = psnus, cop_year = "2024", d2_session = training)
    expect_equal(class(prio_table), "data.frame")
    expect_setequal(names(prio_table), c("orgUnit", "value"))
    expect_equal(typeof(prio_table$orgUnit), "character")
    expect_equal(typeof(prio_table$value), "double")
  })
})
