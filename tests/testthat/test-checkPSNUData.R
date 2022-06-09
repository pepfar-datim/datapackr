context("Can check validation rules")

with_mock_api({
  test_that("Can detect validation rule violations", {

    d <- list()
    d$info$cop_year <- "2022"
    d$info$messages <- MessageQueue()

    d$data$analytics <- tibble::tribble(
      ~dataelement_id, ~fiscal_year, ~psnu_uid, ~categoryoptioncombo_id, ~mechanism_code, ~target_value,
      "DhrLCUBm3bK", "2023", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "20", #TX_NEW
      "HGZY9RNZjRd", "2023", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "10" #TX_CURR
    )
    #Never run more than two threads here to avoid errors in checkPackage
    Sys.setenv("MAX_CORES" = 2L)
    d <- checkPSNUData(d)
    expect_true(NROW(d$tests$vr_rules_check) == 3) #This should trip three rules
    expect_true(sum(d$tests$vr_rules_check$Valid) == 2) #Two should be valid

    violations <- d$tests$vr_rules_check %>%
      dplyr::filter(!Valid)

    expect_named(violations, c("Validation rule", "PSNU", "Mechanism",
    "Formula", "Diff (%)", "Diff (Absolute)", "Valid"))

  })
})


with_mock_api({
  test_that("Can detect no validation rule violations", {

    d <- list()
    d$info$cop_year <- "2022"
    d$info$messages <- MessageQueue()
    Sys.setenv("MAX_CORES" = 1L)
    d$data$analytics <- tibble::tribble(
      ~dataelement_id, ~fiscal_year, ~psnu_uid, ~categoryoptioncombo_id, ~mechanism_code, ~target_value,
      "DhrLCUBm3bK", "2023", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "10", #TX_NEW
      "HGZY9RNZjRd", "2023", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "20" #TX_CURR
    )
    d <- checkPSNUData(d)
    #Return NULL if there are no violations
    expect_null(d$tests$vr_rules_check)

  })
})
