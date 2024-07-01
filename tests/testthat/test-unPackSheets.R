context("test-unPackSheets")

# Mon May 20 16:54:17 2024 ------------------------------
#need to update the xlsx found below touch base with Jason if needed
test_that("Can load sheets if empty ...", {

  d  <- loadDataPack(submission_path = test_sheet("COP24_Data_Pack_Template_minimal.xlsx"),
                     tool = "Data Pack",
                     country_uids = NULL,
                     cop_year = NULL,
                     load_wb = TRUE,
                     load_sheets = TRUE,
                     d2_session = training)
  d$sheets <- NULL
  d <- unPackSheets(d)
  # when d$sheets is explicitly NULL, unPackSheets should call
  # loadSheets and therefore fix the NULL value
  testthat::expect_equal(length(d$sheets), 16) #Changed from 17

})

test_that("Can test sheets are valid...", {

  d <- list()
  d$info$cop_year <- 2024
  d$info$tool <- "Data Pack"

  d$info$schema <-
    tribble(
      ~sheet_name, ~indicator_code, ~col_type, ~value_type,
      "Prioritization", "SNU1", "row_header", "string",
      "Prioritization", "PSNU", "row_header", "string",
      "Prioritization", "IMPATT.PRIORITY_SNU.T", "target", "integer"
    ) %>%
    dplyr::mutate(valid_ages = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_sexes = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_kps = I(list(data.frame(id = NA, name = NA))))


  d$sheets$Prioritization <-
    tribble(
      ~PSNU, ~IMPATT.PRIORITY_SNU.T,
      "_Military Malawi [#Military] [PQZgU9dagaH]", "M",
      "Lilongwe District [#SNU] [ScR9iFKAasW]", "20"
    )

  #We need to pretend we are interactive to get this warning, otherwise, its
  #silent
  options(rlang_interactive = TRUE)
  expect_warning(unPackSheets(d, check_sheets = FALSE, sheets = c("Prioritization", "Cascade")))

})
