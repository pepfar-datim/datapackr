context("Can check validation rules")

# cop23 ----
with_mock_api({
  test_that("Can detect validation rule violations for cop23", {

    d <- list()
    d$info$cop_year <- "2023"
    d$info$messages <- MessageQueue()
    d$info$tool<- "Data Pack"

    d$data$analytics <- tibble::tribble(
      ~dataelement_id, ~fiscal_year, ~psnu_uid, ~categoryoptioncombo_id, ~mechanism_code, ~target_value,
      "DhrLCUBm3bK", "2024", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "20", #TX_NEW
      "HGZY9RNZjRd", "2024", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "10" #TX_CURR
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
  test_that("Can detect no validation rule violations for cop23", {

    d <- list()
    d$info$cop_year <- "2023"
    d$info$messages <- MessageQueue()
    Sys.setenv("MAX_CORES" = 1L)
    d$data$analytics <- tibble::tribble(
      ~dataelement_id, ~fiscal_year, ~psnu_uid, ~categoryoptioncombo_id, ~mechanism_code, ~target_value,
      "DhrLCUBm3bK", "2024", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "10", #TX_NEW
      "HGZY9RNZjRd", "2024", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "20" #TX_CURR
    )
    d <- checkPSNUData(d)
    #Return NULL if there are no violations
    expect_null(d$tests$vr_rules_check)

  })
})


# cop 24 ----
with_mock_api({
  test_that("Can detect validation rule violations for cop24", {

    d <- list()
    d$info$cop_year <- "2024"
    d$info$messages <- MessageQueue()
    d$info$tool<- "Data Pack"

    d$data$analytics <- tibble::tribble(
      ~dataelement_id, ~fiscal_year, ~psnu_uid, ~categoryoptioncombo_id, ~mechanism_code, ~target_value,
      "DhrLCUBm3bK", "2025", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "20", #TX_NEW
      "HGZY9RNZjRd", "2025", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "10" #TX_CURR
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
  test_that("Can detect no validation rule violations for cop23", {

    d <- list()
    d$info$cop_year <- "2024"
    d$info$messages <- MessageQueue()
    Sys.setenv("MAX_CORES" = 1L)
    d$data$analytics <- tibble::tribble(
      ~dataelement_id, ~fiscal_year, ~psnu_uid, ~categoryoptioncombo_id, ~mechanism_code, ~target_value,
      "DhrLCUBm3bK", "2025", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "10", #TX_NEW
      "HGZY9RNZjRd", "2025", "uXwFHXCPYgj", "yWgBf6UOK8d", "12345", "20" #TX_CURR
    )
    d <- checkPSNUData(d)
    #Return NULL if there are no violations
    expect_null(d$tests$vr_rules_check)

  })
})
