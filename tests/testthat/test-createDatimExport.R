context("Create a DATIM export object")

test_that("Can export COP22 data", {


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
