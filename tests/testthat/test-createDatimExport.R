context("Create a DATIM export object")

test_that("Can export COP23 data", {


  d <-
    loadDataPack(
      submission_path = test_sheet("COP23_sample_DataPack_Malawi.xlsx"),
      tool = "Data Pack",
      country_uids = NULL,
      cop_year = NULL,
      load_sheets = TRUE,
      d2_session = training)

  d %<>%
    unPackSheets(., check_sheets = FALSE) %>%
    packForDATIM(., type = "Undistributed MER") %>%
    packForDATIM(., type = "SUBNAT_IMPATT")

  datim_export <- createDATIMExport(d)
  expect_true(all(sapply(datim_export, class) == "character"))

  expect_named(
    datim_export,
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

test_that("Can export COP24 DATIM data", {

  # create mock cop24 pack
  d <- list()
  d$info$cop_year <- 2024
  d$info$has_psnuxim <- TRUE
  d$info$tool <- "Data Pack"

  d$datim$subnat_impatt <- tibble::tribble(
    ~dataElement, ~period, ~orgUnit,  ~categoryOptionCombo, ~attributeOptionCombo,
      "KssDaTsGWnS", "2023Oct", "HxXMyMSODnm", "C8E2J67vSKJ", "HllvX50cXC0",
      "LvwJYdp0Jmr", "2024Oct", "HxXMyMSODnm", "HjSoOWZgbMx", "HllvX50cXC0",
      "P2XNbiNnIqV", "2023Oct", "HxXMyMSODnm", "CeuDnHicKrF", "HllvX50cXC0",
      "SSun4i7nHlV", "2024Oct", "HxXMyMSODnm", "F7tVWi2xkLA", "HllvX50cXC0",
      "lJtpR5byqps", "2023Oct", "HxXMyMSODnm", "C8E2J67vSKJ", "HllvX50cXC0",
      "nF19GOjcnoD", "2023Oct", "HxXMyMSODnm", "CeuDnHicKrF", "HllvX50cXC0"
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
  datim_export <- createDATIMExport(d)
  pop_data <- c("KssDaTsGWnS", "lJtpR5byqps", "nF19GOjcnoD", "P2XNbiNnIqV")
  testthat::expect_equal(
    datim_export %>%
      dplyr::filter(dataElement %in% pop_data) %>%
      NROW(),
    0L
  )

  testthat::expect_true(all(sapply(datim_export, class) == "character"))

  # test all columns are present
  testthat::expect_named(
    datim_export,
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
