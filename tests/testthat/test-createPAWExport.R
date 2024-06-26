test_that("Can export COP24 DATIM data", {

  # create mock cop24 pack
  d <- list()
  d$info$cop_year <- 2024
  d$info$has_psnuxim <- TRUE
  d$info$tool <- "Data Pack"

  d$datim$subnat_impatt <- tibble::tribble(
    ~dataElement, ~period, ~orgUnit,  ~categoryOptionCombo, ~attributeOptionCombo, ~value,
    "KssDaTsGWnS", "2023Oct", "HxXMyMSODnm", "C8E2J67vSKJ", "HllvX50cXC0", 3,
    "LvwJYdp0Jmr", "2024Oct", "HxXMyMSODnm", "HjSoOWZgbMx", "HllvX50cXC0", 6,
    "P2XNbiNnIqV", "2023Oct", "HxXMyMSODnm", "CeuDnHicKrF", "HllvX50cXC0", 7,
    "SSun4i7nHlV", "2024Oct", "HxXMyMSODnm", "F7tVWi2xkLA", "HllvX50cXC0", 23,
    "lJtpR5byqps", "2023Oct", "HxXMyMSODnm", "C8E2J67vSKJ", "HllvX50cXC0", 456,
    "nF19GOjcnoD", "2023Oct", "HxXMyMSODnm", "CeuDnHicKrF", "HllvX50cXC0", 412
  )

  d$datim$UndistributedMER <- tibble::tribble(
    ~dataElement, ~period, ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
    "BeEA8PsZ8Ky", "2024Oct", "HxXMyMSODnm", "BpjQgbuhZoo", "HllvX50cXC0", 34,
    "BeEA8PsZ8Ky", "2024Oct", "HxXMyMSODnm", "DFLZuSpRYKv", "HllvX50cXC0", 25285,
    "BeEA8PsZ8Ky", "2024Oct", "HxXMyMSODnm", "JURc3Uxzcr9", "HllvX50cXC0", 121,
    "BeEA8PsZ8Ky", "2024Oct", "HxXMyMSODnm", "KcI8l7j9oe", "HllvX50cXC0", 147,
  )

  d$datim$OPU <- tibble::tribble(
    ~dataElement, ~period,  ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
    "BeEA8PsZ8Ky", "2024Oct", "HxXMyMSODnm", "BpjQgbuhZoo", "18667", 34,
    "BeEA8PsZ8Ky", "2024Oct", "HxXMyMSODnm", "DFLZuSpRYKv", "18667", 25285,
    "BeEA8PsZ8Ky", "2024Oct", "HxXMyMSODnm", "JURc3Uxzcr9", "18667", 121
  )

  d$datim$prioritizations <- tibble::tribble(
    ~dataElement, ~period, ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
    "r4zbW3owX9n", "2024Oct", "HxXMyMSODnm", "HllvX50cXC0", "HllvX50cXC0", 1,
    "r4zbW3owX9n", "2024Oct", "IaFLxtEwIwk", "HllvX50cXC0", "HllvX50cXC0", 1,
    "r4zbW3owX9n", "2024Oct", "Jm3YTCERxvX", "HllvX50cXC0", "HllvX50cXC0", 1
  )

  # for cop24 data should not have any FY24 subnat target data
  paw_export <- createPAWExport(d)
  pop_data <- c("KssDaTsGWnS", "lJtpR5byqps", "nF19GOjcnoD", "P2XNbiNnIqV")
  testthat::expect_equal(
    paw_export %>%
      dplyr::filter(dataElement %in% pop_data) %>%
      NROW(),
    4L
  )

  testthat::expect_true(all(sapply(paw_export, class) == "character"))

  # test all columns are present
  testthat::expect_named(
    paw_export,
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value"
    )
  )

})
