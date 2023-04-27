context("Can extract Model Data")

test_that("Can extract model data...", {

  d <- list()
  d$info$cop_year <- 2023

  d$sheets$PSNUxIM <-
    tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~ID, ~DataPackTarget, ~`Not PEPFAR`, ~`84753_DSD`,
      "_Military Rwanda [OAIa0wJPpZ2]", "GEND_GBV.PE.T", NA, NA, NA, "_Military Rwanda [OAIa0wJPpZ2]", "600", NA, "1",
      "_Military Rwanda [OAIa0wJPpZ2]", "GEND_GBV.S.T", NA, NA, NA, "_Military Rwanda [OAIa0wJPpZ2]", "120", NA, "1",
      "_Military Rwanda [OAIa0wJPpZ2]", "HTS_RECENT.KP.T", NA, NA, "FSW",
      "_Military Rwanda [OAIa0wJPpZ2]|FSW", "12", NA, "1",
      "_Military Rwanda [OAIa0wJPpZ2]", "HTS_RECENT.T", "15-24",
      "Female", NA, "_Military Rwanda [OAIa0wJPpZ2]|15-24|Female", "3", NA, "1"
    )

  # test that header cols are removed from output
  h_cols <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop", "DataPackTarget")
  res <- extractDataPackModel(d)
  testthat::expect_false(unique(h_cols %in% names(res)))

  # expect only mech codes code element as attributeOptionCombo
  m_col_pos <- which(grepl("^\\d{4,}_(DSD|TA)$", names(d$sheets$PSNUxIM)))
  m_cols <- gsub("_DSD", "", names(d$sheets$PSNUxIM[, m_col_pos]))
  res <- extractDataPackModel(d)
  testthat::expect_true(unique(res$attributeOptionCombo %in% m_cols))

  # we expect org unit to be the the code included in PSNU
  o_unit <- stringr::str_extract(d$sheets$PSNUxIM$PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")
  testthat::expect_true(unique(res$orgUnit %in% o_unit))

})
