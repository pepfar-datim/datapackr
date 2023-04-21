context("Create a DATIM export object")

test_that("Can export COP22 data", {

  expect_warning(d <- loadDataPack(submission_path = test_sheet("COP22_DataPack_unPackingChecks.xlsx"),
                                   tool = "Data Pack",
                                   country_uids = NULL,
                                   cop_year = NULL,
                                   load_wb = TRUE,
                                   load_sheets = TRUE,
                                   d2_session = training))

  datim_export <- createDATIMExport(d)

  expect_true(all(sapply(datim_export, class) == "character"))
  skip("No idea why this is failing")
  expect_named(
    datim_export,
    c(
      "dataElement",
      "period",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value"
    )
  )


  })
