context("test-unPackSheets")


test_that("Can load sheets if empty ...", {

  d  <- loadDataPack(submission_path = test_sheet("COP21_Data_Pack_Template.xlsx"),
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
  testthat::expect_equal(length(d$sheets), 18)

})

test_that("Can test sheets are valid...", {

  d <- list()
  d$info$cop_year <- 2022
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

  # TODO: need to figure out right warning equivalent
  # test that interactive warning is produced
  # couldn't match warning perfectly so expectation is some warning
  testthat::expect_warning(unPackSheets(d, check_sheets = FALSE, sheets = c("Prioritization", "Cascade")))

})
