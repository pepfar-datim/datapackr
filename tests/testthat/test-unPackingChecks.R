test_that("Can detect invalid comment types ...", {

  #Note: A warning is thrown here on the command line  for "invalid parameter"
  # Does not have any impact on the parsing, but documenting it nonetheless.
  expect_warning(d <- loadDataPack(submission_path = test_sheet("COP24_DataPack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = TRUE,
                    load_sheets = TRUE,
                    d2_session = training))

  d <- checkToolComments(d)

  expect_true(d$info$has_comments_issue)
  expect_true(d$info$has_error)


  d  <- loadDataPack(submission_path = getTemplate("COP23_Data_Pack_Template.xlsx"),
                     tool = "Data Pack",
                     country_uids = NULL,
                     cop_year = NULL,
                     load_wb = TRUE,
                     load_sheets = TRUE,
                     d2_session = training) %>%
    checkToolComments()

  # should expect no issues so FALSE
  expect_false(d$info$has_comments_issue)
})

test_that("Can detect external links in a file ...", {
  expect_warning(d <- loadDataPack(submission_path = test_sheet("COP24_DataPack_unPackingChecks.xlsx"),
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    load_wb = TRUE,
                    load_sheets = FALSE,
                    d2_session = training))

  d <- checkToolConnections(d)

  expect_true(d$info$has_external_links)
})

test_that("Can check Tool structure...", {
  d  <- loadDataPack(submission_path = getTemplate("COP23_Data_Pack_Template.xlsx"),
                     tool = "Data Pack",
                     country_uids = NULL,
                     cop_year = NULL,
                     d2_session = training)

  expect_equal(NROW(d$tests$missing_sheets), 0L)

  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = getTemplate("COP23_Data_Pack_Template.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)
  openxlsx::removeWorksheet(wb, "PMTCT")
  openxlsx::saveWorkbook(wb, file = template_copy, overwrite = TRUE)
  d <- loadDataPack(submission_path = template_copy,
                    tool = "Data Pack",
                    country_uids = NULL,
                    cop_year = NULL,
                    d2_session = training)
  d <- checkToolStructure(d)
  expect_equal(NROW(d$tests$missing_sheets), 1L)
  expect_true(grepl("MISSING SHEETS", d$info$messages$message))
})

skip("DP-1322: Need an unpacking artifact to test with for COP24")
test_that("Can check sheet data...", {


    d <- loadDataPack(submission_path = test_sheet("COP24_DataPack_unPackingChecks.xlsx"),
                                           tool = "Data Pack",
                                           country_uids = NULL,
                                           cop_year = NULL,
                                           load_wb = FALSE,
                                           load_sheets = TRUE,
                                           d2_session = training)

   d <- checkSheetData(d)

   expect_true("duplicate_rows" %in% names(d$tests))
   expect_true("missing_cols" %in% names(d$tests))
   expect_true("duplicate_columns" %in% names(d$tests))
   expect_true("columns_out_of_order" %in% names(d$tests))
   expect_true("non_numeric" %in% names(d$tests))
   expect_true("negative_values" %in% names(d$tests))
   expect_true("decimal_values" %in% names(d$tests))
   expect_true("invalid_orgunits" %in% names(d$tests))
   expect_true("invalid_prioritizations" %in% names(d$tests))
   expect_true("altered_formulas" %in% names(d$tests))

   #expect_true("defunct_disaggs" %in% names(d$tests))

   expect_true(any(grepl("In tab GEND: DUPLICATE ROWS", d$info$messages$message)))
   expect_true(any(grepl("In tab GEND, MISSING COLUMNS", d$info$messages$message)))
   expect_true(any(grepl("In tab GEND, DUPLICATE COLUMNS", d$info$messages$message)))
   expect_true(any(grepl("In tab GEND, OUT OF ORDER COLUMNS", d$info$messages$message)))
   expect_true(any(grepl("In tab GEND: NON-NUMERIC VALUES", d$info$messages$message)))
   expect_true(any(grepl("In tab GEND: NEGATIVE VALUES", d$info$messages$message)))
   expect_true(any(grepl("In tab GEND: DECIMAL VALUES", d$info$messages$message)))
   expect_true(any(grepl("In tab GEND, INVALID OR BLANK ORG UNITS", d$info$messages$message)))
   expect_true(any(grepl("ERROR! In tab Prioritization: INVALID PRIORITIZATIONS", d$info$messages$message)))
   expect_true(any(grepl("ALTERED FORMULAS", d$info$messages$message)))
   expect_true(any(grepl("ERROR! In tab OVC: INVALID DISAGGS", d$info$messages$message)))

   expect_equal(nrow(d$tests$duplicate_rows), 4L)
   expect_equal(nrow(d$tests$missing_cols), 1L)
   expect_equal(nrow(d$tests$duplicate_columns), 2L)
   expect_equal(nrow(d$tests$columns_out_of_order), 2L)
   expect_equal(nrow(d$tests$non_numeric), 819L)
   expect_equal(nrow(d$tests$negative_values), 6L)
   expect_equal(nrow(d$tests$decimal_values), 29L)
   expect_equal(nrow(d$tests$invalid_orgunits), 2L)
   expect_equal(nrow(d$tests$invalid_prioritizations), 4L)
   expect_equal(nrow(d$tests$altered_formulas), 18L)
   expect_equal(nrow(d$tests$defunct_disaggs), 12L)

  expect_true(d$info$has_error)
})




# check decimal values -----
test_that("Can check decimal values", {

  # choose minimal sheets to test
  test_sheets <- c(
    "Prioritization"
  )

  # create minimal schema data
  d <- list()
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


  d$info$cop_year <- 2024
  d$sheets$Prioritization <-
    tribble(
      ~PSNU, ~IMPATT.PRIORITY_SNU.T,
      "_Military Malawi [#Military] [PQZgU9dagaH]", "M",
      "Lilongwe District [#SNU] [ScR9iFKAasW]", "20",
      "Dowa District [#SNU] [zphK9WV8JB4]", "NA"
    )

  # test no false positive
  res <- checkDecimalValues(d = d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  d$sheets$Prioritization <- d$sheets$Prioritization %>%
    add_row(!!!setNames(c("testing district", "1.5"), names(.)))
  res <- checkDecimalValues(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$result$value, 1.5)
  expect_equal(res$lvl, "WARNING")

  rm(res, d)


})

# check non numeric values ----
test_that("Can check non numeric values", {

  test_sheets <- c(
    "Prioritization"
  )

  # create minimal schema data
  d <- list()
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


  d$info$cop_year <- 2024
  d$sheets$Prioritization <-
    tribble(
      ~PSNU, ~IMPATT.PRIORITY_SNU.T,
      "_Military Malawi [#Military] [PQZgU9dagaH]", "M",
      "Lilongwe District [#SNU] [ScR9iFKAasW]", "20"
    )

  # test no errors/warnings
  res <- checkNonNumeric(d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  # we add NA to test the dropping of NA values
  # note that any values that are interpreted as character "NAs" are not dropped
  d$sheets$Prioritization <- d$sheets$Prioritization %>%
    add_row(!!!setNames(c("Some other District [#SNU] [zphK9WV8J78]", "blah"), names(.))) %>%
    add_row(!!!setNames(c("Dowa District [#SNU] [zphK9WV8JB4]", NA), names(.))) %>%
    add_row(!!!setNames(c("Another district [#SNU] [zphK9WV8JB9]", "NA"), names(.)))

  res <- checkNonNumeric(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 2L)
  expect_equal(res$lvl, "WARNING")
  expect_equal(res$has_error, FALSE)

  rm(res, d)


})

# check negative values ----
test_that("Can check negative values", {

  test_sheets <- c(
    "Prioritization"
  )

  # create minimal schema data
  d <- list()
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

  d$info$cop_year <- 2024
  # test no errors/warnings
  res <- checkNegativeValues(d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  # we add a negative value
  d$sheets$Prioritization <- d$sheets$Prioritization %>%
    add_row(!!!setNames(c("Some other District [#SNU] [zphK9WV8J78]", "-6"), names(.)))

  res <- checkNegativeValues(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)

  rm(res, d)


})

# check dupe rows ----
test_that("Can check dupe rows", {

    test_sheets <- c(
      "Prioritization"
    )

    # create minimal schema data
    d <- list()
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

    # test no errors/warnings
    res <- checkDupeRows(d, sheets = test_sheets)
    expect_null(res$result)
    expect_equal(res$has_error, FALSE)
    rm(res)

    # test positive flag
    # we add a duplicate row on PSNU
    d$sheets$Prioritization <- d$sheets$Prioritization %>%
      add_row(!!!setNames(c("Lilongwe District [#SNU] [ScR9iFKAasW]", "10"), names(.)))

    res <- checkDupeRows(d = d, sheets = test_sheets)

    expect_equal(nrow(res$result), 1L)
    expect_equal(res$lvl, "ERROR")
    expect_equal(res$has_error, TRUE)

    rm(res, d)


  })

# check missing cols ----
test_that("Can check missing cols", {

  test_sheets <- c(
    "Prioritization"
  )

  # create minimal schema data
  d <- list()
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
      ~SNU1, ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", NA, "M", "Military",
      "Central Region", "Lilongwe District [#SNU] [ScR9iFKAasW]", "4", "4", "Sustained",
      "Central Region", "Dowa District [#SNU] [zphK9WV8JB4]", "4", NA, "Not a PSNU"
    )

  # test no errors/warnings
  res <- checkMissingCols(d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  # we remove columns
  d$sheets$Prioritization <- d$sheets$Prioritization[, 1:3]

  res <- checkMissingCols(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$result$indicator_code, "IMPATT.PRIORITY_SNU.T")
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)

  rm(res, d)


})

# check dupe cols ----
test_that("Can check dupe cols", {

  test_sheets <- c(
    "Prioritization"
  )

  # create minimal schema data
  d <- list()
  d$info$schema <-
    tribble(
      ~sheet_name, ~col, ~indicator_code, ~col_type, ~value_type,
      "Prioritization", 1, "SNU1", "row_header", "string",
      "Prioritization", 2,  "PSNU", "row_header", "string",
      "Prioritization", 3, "IMPATT.PRIORITY_SNU.T_1", "past", "integer",
      "Prioritization", 4,  "IMPATT.PRIORITY_SNU.T", "target", "integer"
    ) %>%
    dplyr::mutate(valid_ages = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_sexes = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_kps = I(list(data.frame(id = NA, name = NA))))


  d$sheets$Prioritization <-
    tribble(
      ~SNU1, ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", NA, "M", "Military",
      "Central Region", "Lilongwe District [#SNU] [ScR9iFKAasW]", "4", "4", "Sustained",
      "Central Region", "Dowa District [#SNU] [zphK9WV8JB4]", "4", NA, "Not a PSNU"
    )

  # test no errors/warnings
  res <- checkDupeCols(d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  # we remove columns
  d$sheets$Prioritization <-
    cbind(
      d$sheets$Prioritization,
      tribble(~IMPATT.PRIORITY_SNU.T, "a", "b", "c")
    )

  res <- checkDupeCols(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$result$indicator_code, "IMPATT.PRIORITY_SNU.T")
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)

  rm(res, d)


})

# check out of order cols ----
test_that("Can check out of order cols", {

  test_sheets <- c(
    "Prioritization"
  )

  # create minimal schema data
  d <- list()
  d$info$schema <-
    tribble(
      ~sheet_name, ~col, ~indicator_code, ~col_type, ~value_type,
      "Prioritization", 1, "SNU1", "row_header", "string",
      "Prioritization", 2,  "PSNU", "row_header", "string",
      "Prioritization", 3, "IMPATT.PRIORITY_SNU.T_1", "past", "integer",
      "Prioritization", 4,  "IMPATT.PRIORITY_SNU.T", "target", "integer"
    ) %>%
    dplyr::mutate(valid_ages = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_sexes = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_kps = I(list(data.frame(id = NA, name = NA))))


  d$sheets$Prioritization <-
    tribble(
      ~SNU1, ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", NA, "M", "Military",
      "Central Region", "Lilongwe District [#SNU] [ScR9iFKAasW]", "4", "4", "Sustained",
      "Central Region", "Dowa District [#SNU] [zphK9WV8JB4]", "4", NA, "Not a PSNU"
    )

  # test no errors/warnings
  res <- checkOutOfOrderCols(d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  # we reverse order of columns
  d$sheets$Prioritization <- d$sheets$Prioritization[, order(rev(names(d$sheets$Prioritization)))]

  res <- checkOutOfOrderCols(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 3L)
  expect_equal(res$lvl, "WARNING")
  expect_equal(res$has_error, FALSE)

  rm(res, d)


})

# check invalid org units ----
test_that("Can check invalid org units", {

  test_sheets <- c(
    "Cascade"
  )

  # create minimal sheet needed
  d <- list()
  d$sheets$Cascade <-
    tribble(
      ~SNU1, ~PSNU, ~Age, ~Sex, ~ID, ~POP_EST.T_1,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", "<01",
      "Female", "_Military Malawi [#Military] [PQZgU9dagaH]|<01|Female", NA,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", "<01",
      "Male", "_Military Malawi [#Military] [PQZgU9dagaH]|<01|Male", NA,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", "01-04",
      "Female", "_Military Malawi [#Military] [PQZgU9dagaH]|01-04|Female", NA,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", "01-04",
      "Male", "_Military Malawi [#Military] [PQZgU9dagaH]|01-04|Male", NA,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", "05-09",
      "Female", "_Military Malawi [#Military] [PQZgU9dagaH]|05-09|Female", NA
    )

  d$info$cop_year <- 2024
  # test no errors/warnings
  res <- checkInvalidOrgUnits(d = d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  # add bad PSNU
  d$sheets$Cascade <- d$sheets$Cascade %>%
    add_row(!!!setNames(c("_Military Malawi", "_Military Malawi [#Military] [BLAHBLAH]",
                          "05-09", "Female",
                          "_Military Malawi [#Military] [PQZgU9dagaH]|05-09|Female",
                          NA), names(.)))

  res <- checkInvalidOrgUnits(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)

  rm(res, d)


})

test_that("Can check invalid prioritizations", {

  test_sheets <- c(
    "Prioritization"
  )

  # create minimal sheet needed
  d <- list()
  d$sheets$Prioritization <-
    tribble(
      ~SNU1, ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", NA, "M", "Military",
      "Central Region", "Lilongwe District [#SNU] [ScR9iFKAasW]", "4", "4", "Sustained"
    )
  d$info$cop_year <- 2024
  d$info$operating_unit$ou_uid <- "lZsCb6y0KDX"

  # test no errors/warnings
  res <- checkInvalidPrioritizations(d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive flag
  # add row with NA value that triggers invalid prioritization
  d$sheets$Prioritization <- d$sheets$Prioritization %>%
    add_row(!!!setNames(c("Central Region", "Dowa District [#SNU] [zphK9WV8JB4]",
                          "4", NA, "Not a PSNU"), names(.)))

  res <- checkInvalidPrioritizations(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)

  rm(res, d)


})

# check invalid prioritization ----
test_that("Can check invalid prioritizations COP23", {

  test_sheets <- c(
    "Prioritization"
  )
 # create minimal sheet needed
  d <- list()
  d$sheets$Prioritization <-
    tribble(
      ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "_Military Malawi[PQZgU9dagaH]", NA, "M", "Military",
      "Dedza District [PekKUkKHAzY]", "4", "4", "Sustained"
    )
  d$info$cop_year <- 2023
  d$info$operating_unit$ou_uid <- "lZsCb6y0KDX"


  # test no errors/warnings
  res <- checkInvalidPrioritizations(d, sheets = test_sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)


  # test positive flag
  # add row with NA value that triggers invalid prioritization
  d$sheets$Prioritization <- d$sheets$Prioritization %>%
    add_row(!!!setNames(c("Kasungu District [JQKLKuVuY61]", "4", NA, "Not a PSNU"), names(.)))

  res <- checkInvalidPrioritizations(d = d, sheets = test_sheets)

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)



  #In the list of valid PSNUs but at the wrong level
  #Take a DSNU from Eswatini which should not have an assigned prioritization
  d <- list()
  d$sheets$Prioritization <-
    tribble(
      ~SNU1, ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "Hhohho", "Hhohho [qYzGABaWyCf]", "4", "4", "Sustained",
      "Lobamba", "Lobamba [ciLrwlyi1dv]", "4", "4", "Sustained"
    )
  d$info$cop_year <- 2023
  d$info$operating_unit$ou_uid <- "V0qMZH29CtN"

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)

  #It looks like a PSNU, but its not
  d <- list()
  d$sheets$Prioritization <-
    tribble(
      ~SNU1, ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "Hhohho", "Hhohho [qYzGABaWyCf]", "4", "4", "Sustained",
      "Bogus PSNU", "Bogus PSNU [ARVh1xCeJhU]", "4", "4", "Sustained"
    )
  d$info$cop_year <- 2023
  d$info$operating_unit$ou_uid <- "V0qMZH29CtN"

  expect_equal(nrow(res$result), 1L)
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)


})

# check formulas ----
test_that("Can check formulas", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$cop_year <- "2024"
  d$keychain$submission_path <- test_sheet("COP24_DataPack_unPackingChecks.xlsx")

  test_sheets <- c(
    "Prioritization"
  )

  # create minimal schema data
  d$info$schema <-
    tribble(
      ~sheet_num, ~sheet_name, ~col, ~indicator_code, ~col_type, ~value_type, ~formula,
      3, "Prioritization", 4, "IMPATT.PRIORITY_SNU.T", "target", "integer",
      'IF(LEFT($B\\d+,4)=\"_Mil\",\"M\",IF(SUM($C\\d+)=0,\"\",$C\\d+))',
      3, "Prioritization", 5, "PRIORITY_SNU.translation", "reference", "string",
      'IF(LEFT($B\\d+,4)=\"_Mil\",\"M\",IF(SUM($C\\d+)=0,\"\",$C\\d+))'
    ) %>%
    dplyr::mutate(valid_ages = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_sexes = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_kps = I(list(data.frame(id = NA, name = NA))))


  d$sheets$Prioritization <-
    tribble(
      ~SNU1, ~PSNU, ~IMPATT.PRIORITY_SNU.T_1, ~IMPATT.PRIORITY_SNU.T, ~PRIORITY_SNU.translation,
      "_Military Malawi", "_Military Malawi [#Military] [PQZgU9dagaH]", NA, "M", "Military",
      "Central Region", "Lilongwe District [#SNU] [ScR9iFKAasW]", "4", "4", "Sustained"
    )

  # test error kicks because of incorrect formulas
  res <- checkFormulas(d, sheets = test_sheets)
  expect_equal(nrow(res$result), 3L)
  expect_equal(res$lvl, "WARNING")
  expect_equal(res$has_error, FALSE)

  rm(res, d)


})

# check disaggs ----
test_that("Can check disaggs", {

  #skip("Defunct disaggs checks needs to be fixed")
  # create minimal schema data
  d <- list()
  d$info$cop_year <- 2024
  d$info$schema <-
    tribble(
      ~sheet_num, ~sheet_name, ~col, ~indicator_code, ~col_type, ~value_type,
      8, "VMMC", 1, "SNU1",  "row_header",     "string",
      8, "VMMC", 2, "PSNU",  "row_header",     "string",
      8, "VMMC", 3, "Age",   "row_header",     "string",
      8, "VMMC", 4, "Sex",   "row_header",     "string",
      8, "VMMC", 5, "ID",    "row_header",     "string",
      8, "VMMC", 17, "VMMC_CIRC_SUBNAT.T", "target", "integer"
    ) %>%
    dplyr::mutate(valid_ages = I(list(tibble(id = c("ttf9eZCHsTU", "GaScV37Kk29"), name = c("15-19", "20-24"))))) %>%
    dplyr::mutate(valid_sexes = I(list(tibble(id = c("Qn0I5FbKQOA"), name = "Male")))) %>%
    dplyr::mutate(valid_kps = I(list(data.frame(id = NA, name = NA))))


  # create minimal sheet needed
  # vmcc with proper data note that org units are valid
  d$sheets$VMMC <-
    tribble(
      ~SNU1, ~PSNU, ~Age, ~Sex, ~ID, ~VMMC_CIRC_SUBNAT.T, #~VMMC_CIRC.R,
      "Angola", "Bengo [#SNU] [uXwFHXCPYgj]", "15-19", "Male", "Bengo [#SNU] [uXwFHXCPYgj]|15-19|Male", "41146",
      "Angola", "Bengo [#SNU] [uXwFHXCPYgj]", "20-24", "Male", "Bengo [#SNU] [uXwFHXCPYgj]|20-24|Male", "36043",
    )

  # next test
  sheets <- c("VMMC")

  # test no issues
  res <- checkDisaggs(d = d, sheets = sheets)
  expect_null(res$result)
  expect_equal(res$has_error, FALSE)
  rm(res)

  # test positive error
  # vmcc should not be female as that is an invalid disagg
  d$sheets$VMMC <-
    tribble(
      ~SNU1, ~PSNU, ~Age, ~Sex, ~ID, ~VMMC_CIRC_SUBNAT.T, #~VMMC_CIRC.R,
      "Angola", "Bengo [#SNU] [uXwFHXCPYgj]", "15-19", "Female", "Bengo [#SNU] [uXwFHXCPYgj]|15-19|Male", "41146",
      "Angola", "Bengo [#SNU] [uXwFHXCPYgj]", "20-24", "Male", "Bengo [#SNU] [uXwFHXCPYgj]|20-24|Male", "36043",
    )


  res <- checkDisaggs(d = d, sheets = sheets)
  expect_equal(nrow(res$result), 1L)
  expect_equal(res$lvl, "ERROR")
  expect_equal(res$has_error, TRUE)

})


test_that("Can check index header columns exist", {

  d <- list()
  d$info$schema <- datapackr::cop24_data_pack_schema
  d$info$messages <- MessageQueue()

  #Get the HTS sheet headers. This one should pass

  hts_col_headers <- d$info$schema %>%
    dplyr::filter(sheet_name == "HTS",
                  col_type == "row_header",
                  !indicator_code %in% c("SNU1", "ID")) %>%
    dplyr::pull(indicator_code)

  mock_data <- data.frame(t(rep("abc123", length(hts_col_headers))))
  hts_df <- mock_data
  names(hts_df) <- hts_col_headers

  d$sheets$HTS <- hts_df

  #Fake another one

  bogus_sheet <- mock_data

  d$sheets$KP_MAT <- bogus_sheet

  d <- checkToolEmptySheets(d, sheets = c("HTS", "KP_MAT"))
  expect_true(is.data.frame(d$tests$missing_index_columns))
  expect_setequal(names(d$tests$missing_index_columns), c("sheet_name"))
  expect_equal(d$tests$missing_index_columns$sheet_name, "KP_MAT")

  })
