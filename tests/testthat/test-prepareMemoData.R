context("prepare-memo-data")

with_mock_api({


  test_that("We can prepare memo metadata", {
    d <- list()
    d$info$country_uids <- "cDGPF739ZZr"
    d$info$cop_year <- "2022"
    d$data$analytics <- data.frame(psnu_uid = "uXwFHXCPYgj", prioritization = "2")
    d <- prepareMemoMetadata(d, "datapack", d2_session = training)
    expect_true(is.list(d$memo))
    expect_setequal(names(d$memo), c("structure", "inds", "partners_agencies", "datapack"))

    expect_true(is.data.frame(d$info$psnus))
    expect_setequal(names(d$info$psnus), c("ou", "country_name", "snu1", "psnu", "psnu_uid"))

    expect_true(is.list(d$memo$datapack$prios))
    expect_setequal(names(d$memo$datapack$prios), c("orgUnit", "prioritization", "value"))
    expect_setequal(names(d$memo$partners_agencies), c("Mechanism", "Partner", "Agency"))

    expect_true(is.list(d$memo$inds))
    expect_setequal(names(d$memo$inds), c("name", "id", "numerator", "denominator"))
  })})
