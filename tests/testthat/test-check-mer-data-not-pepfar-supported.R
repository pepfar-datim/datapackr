context("test-check-mer-data-non-pepfar-supported")

test_that("Can flag MER data in Not PEPFAR supported PSNUs", {

  d <- list()
  d$info$cop_year <- "2024"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$tests <- list()
  d$data$analytics <- tibble::tribble(
    ~psnu, ~dataelement_id, ~dataelement_name, ~mechanism_desc, ~prioritization,
    "Foobar", "BeEA8PsZ8Ky", "TX_CURR (N, DSD, Age/Sex/HIVStatus) TARGET: Receiving ART",
    "Fake Mech", "Not PEPFAR Supported"
  )
  d$info$has_error <- FALSE

  d <- checkNotPEPFARSupportedPSNUs(d)
  testthat::expect_true(grepl("Not PEPFAR supported", d$info$messages$message))
  testthat::expect_true(NROW(d$tests$mer_data_not_pepfar_supported) == 1)
  testthat::expect_false(d$info$has_error)

})

test_that("Can pass MER data in PEPFAR supported PSNUs", {

  d <- list()
  d$info$cop_year <- "2024"
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$tests <- list()
  d$data$analytics <- tibble::tribble(
    ~psnu, ~dataelement_id, ~dataelement_name, ~mechanism_desc, ~prioritization,
    "Foobar", "BeEA8PsZ8Ky", "TX_CURR (N, DSD, Age/Sex/HIVStatus) TARGET: Receiving ART",
    "Fake Mech", "Scale-up: Aggressive"
  )
  d$info$has_error <- FALSE

  d <- checkNotPEPFARSupportedPSNUs(d)
  testthat::expect_true(length(d$info$messages$message) == 0)
  testthat::expect_null(d$tests$mer_data_not_pepfar_supported)
  testthat::expect_false(d$info$has_error)

})
