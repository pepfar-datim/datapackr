context("Create a PAW export object")

test_that("Can export COP24 PAW data", {

  # create mock cop24 pack
  d <- list()
  d$info$cop_year <- 2024
  d$info$has_psnuxim <- TRUE
  d$info$tool <- "Data Pack"

  d$data$MER <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Female", NA, 354,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "01-09", "Male", NA, 371,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "TX_CURR.T", "10-14", "Female", NA, 299
    )
  d$data$SUBNAT_IMPATT <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 1,
      "Centre [IaFLxtEwIwk]", "IaFLxtEwIwk", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 1,
      "Est [KtMNtCHNQDJ]", "KtMNtCHNQDJ", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 1,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "DIAGNOSED_SUBNAT.T_1", "<01", "Female", NA, 1,
      "Centre [IaFLxtEwIwk]", "IaFLxtEwIwk", "Cascade", "POP_EST.T_1", "<01", "Female", NA, 1,
      "Adamaoua [xkPoe5mhHE5]", "xkPoe5mhHE5", "Cascade", "HIV_PREV.T_1", "<01", "Female", NA, 1
    )
  d$data$SNUxIM <-
    tibble::tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~mech_code, ~support_type, ~value,
      "Adamaoua [xkPoe5mhHE5]", "GEND_GBV.PE.T", NA, NA, NA, "xkPoe5mhHE5", "81580", "DSD", 150,
      "Adamaoua [xkPoe5mhHE5]", "GEND_GBV.S.T", NA, NA, NA, "xkPoe5mhHE5", "81580", "DSD", 300,
      "Adamaoua [xkPoe5mhHE5]", "HTS.Index.Neg.T", "01-09", "Female", NA, "xkPoe5mhHE5", "81580", "DSD", 485,
      "_Military Cameroon [eky41asIrS2]", "TX_TB.D.New.Neg.T", "15+", "Female", NA, "eky41asIrS2", "86656", "DSD", 239,
      "_Military Cameroon [eky41asIrS2]", "TX_TB.D.New.Neg.T", "15+", "Male", NA, "eky41asIrS2", "86656", "DSD", 179
    )

  # PAW Data is packed inside the create export function
  paw_export <- createPAWExport(d)
  expect_true(all(sapply(paw_export, class) == "character"))

  # test all columns are present
  expect_named(
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
