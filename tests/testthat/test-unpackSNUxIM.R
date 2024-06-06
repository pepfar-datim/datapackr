context("Unpack a PSNUxIM file")

test_that("Can error if d$data$PSNUxIM is not a data frame", {

  d <- list()
  expect_error(checkHasPSNUxIM(d))

})

test_that("Can check empty PSNUxIM", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$data$SNUxIM <- data.frame(foo = c(NA), bar = c(NA))
  d$info$tool <- "Data Pack"
  d <- checkHasPSNUxIM(d)
  expect_false(d$info$has_psnuxim)
  expect_true(d$info$needs_psnuxim)
  expect_true(stringr::str_detect(d$info$messages$message, "Your Data Pack needs a new PSNUxIM tab."))
})

test_that("Can check PSNUxIM with data exists", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$data$SNUxIM <- data.frame(foo = c(1, 2), bar = c(3, 4))
  d$info$tool <- "Data Pack"
  d <- checkHasPSNUxIM(d)
  expect_true(d$info$has_psnuxim)
  expect_true(is.null(d$info$needs_psnuxim))

})

test_that("Can extract OPU SNUxIM combos", {

  d <- list()
  d$info$tool <- "OPU Data Pack"
  d$info$messages <- MessageQueue()

  test_data  <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN] ", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "TX_NEW.T", "<1", "M", NA
  )

  d$data$SNUxIM <- test_data

  d <- extractSNUxIMCombos(d)

  expect_named(
    d$data$PSNUxIM_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )


})


test_that("Can extract Datapack SNUxIM combos", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()

  test_data  <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "TX_NEW.T", "<1", "M", NA
  )

  d$data$MER <- test_data %>%
    dplyr::mutate(psnuid = c("NYWv44mLzDN", "iS02nBlAiva"))
  d$data$SNUxIM <- test_data

  d <- extractSNUxIMCombos(d)

  expect_named(
    d$data$PSNUxIM_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_equal(NROW(d$data$missingCombos), 0L)
  expect_named(
    d$tests$missing_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_false(d$info$missing_psnuxim_combos)

})

test_that("Can extract missing Datapack SNUxIM combos", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()

  test_data  <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "TX_NEW.T", "<1", "M", NA
  )
  d$data$SNUxIM <- test_data

  d$data$MER <- tibble::tribble(
    ~PSNU, ~psnuid, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN]", "NYWv44mLzDN", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "iS02nBlAiva", "TX_NEW.T", "<1", "M", NA,
    "Kebab District [kKmyE7NrhZb]", "kKmyE7NrhZb", "TX_CURR.T", "15-19", "F", NA
  )


  d <- extractSNUxIMCombos(d)

  expect_named(
    d$data$PSNUxIM_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_equal(NROW(d$data$missingCombos), 1L)
  expect_named(
    d$tests$missing_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_identical(d$data$missingCombos, d$data$MER[3, ])

  expect_true(d$info$missing_psnuxim_combos)

})


test_that("Can indentify duplicate rows in PSNUxIM tab", {
  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()

  d$data$SNUxIM <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~DataPackTarget,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-49", "F", NA, 10,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-49", "F", NA, 10)

   d <- extractDuplicateRows(d, "PSNUxIM")
   expect_equal(NROW(d$tests$duplicate_rows), 1L)
   expect_true(all(d$tests$duplicate_rows$sheet == "PSNUxIM"))
   expect_true(d$info$has_error)

})

test_that("Can get columns to keep", {
  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$schema <- datapackr::cop22OPU_data_pack_schema
  cols_to_keep <- getColumnsToKeep(d, sheet = "PSNUxIM")
  expect_setequal(names(cols_to_keep), names(cop22OPU_data_pack_schema))
})


skip("This test is broken for COP24")
with_mock_api({
test_that("Can detect missing right side formulas", {


  generation_list <- c("Malawi")

  pick <- datapackr::cop24_datapack_countries %>%
    dplyr::filter(datapack_name %in% generation_list) %>%
    dplyr::arrange(datapack_name)

  output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
  dir.create(output_folder)

  #This should produce a tool with no formulas on the right hand side.
  d <- packTool(tool = "PSNUxIM",
                datapack_name = pick$datapack_name[1],
                country_uids = unlist(pick$country_uids[1]),
                snuxim_model_data_path = test_sheet("COP23_SNUxIM_Model_Random.rds"),
                template_path = NULL,
                cop_year = 2024,
                output_folder = output_folder,
                results_archive = FALSE,
                expand_formulas = FALSE,
                d2_session = training)

  sheet <- "PSNUxIM"
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  cols_to_keep <- getColumnsToKeep(d, sheet)
  header_cols <- getHeaderColumns(cols_to_keep, sheet)
  d$data$SNUxIM <-
    readxl::read_excel(
      path = d$info$output_file,
      sheet = sheet,
      range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
      col_types = "text",
      .name_repair = "minimal"
    )

  blank_cols_idx <- which(names(d$data$SNUxIM) == "")
  #Read from the output file, which has not been opened, thus no formulas
  parsed_cells <-  tidyxl::xlsx_cells(path = d$info$output_file,
                                      sheets = "PSNUxIM",
                                      include_blank_cells = TRUE)

  d <- testMissingRightSideFormulas(d,
                                 cols_to_keep,
                                 header_cols,
                                 header_row,
                                 blank_cols_idx,
                                 parsed_cells = parsed_cells)

  expect_named(d$tests$psnuxim_missing_rs_fxs, c("col", "row", "formula", "col_letter"))
  expect_true(NROW(d$tests$psnuxim_missing_rs_fxs) > 1L)
})
})



















test_that("Can drop duplicated PSNUxIM columns", {
  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  test_data <- data.frame(foo = c(1L, 1L), foo2 = c(2L, 2L), bar = c(3L, 3L))
  d$data$SNUxIM <- test_data
  names(d$data$SNUxIM) <- c("foo", "foo", "bar")
  expect_warning(d <- dropDuplicatedPSNUxIMColumns(d))
  expect_true(NCOL(d$data$SNUxIM) == 2L)
  expect_named(d$data$SNUxIM, c("foo", "bar"))
  expect_identical(d$data$SNUxIM, test_data[, c(1, 3)])
})


test_that("Can drop invalid mechanism columns", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  d$info$schema <- cop22OPU_data_pack_schema
  cols_to_keep <- getColumnsToKeep(d, sheet = "PSNUxIM")

  #Do nothing if the columns are OK
  test_data <- tibble::tribble(
    ~`12345_DSD`, ~`45678_DSD`, ~`99999_TA`, ~"Not PEPFAR",
    1, 2, 3, 4
  )

  d$data$SNUxIM <- test_data
  d <- dropInvalidMechColumns(d, cols_to_keep)

  expect_identical(d$data$SNUxIM, test_data)


  test_data <- tibble::tribble(
    ~`ABC_DSD`, ~`45678_DSD`, ~`99999_TA`, ~"Not PEPFAR",
    1, 2, 3, 4
  )

  d$data$SNUxIM <- test_data
  d <- dropInvalidMechColumns(d, cols_to_keep)

  expect_identical(d$data$SNUxIM, test_data[, 2:4])
  expect_identical(d$tests$invalid_mech_headers$invalid_mech_headers, "ABC_DSD")
  expect_true(d$info$has_error)
  expect_true(grepl("INVALID COLUMN HEADERS", d$info$messages$message))
  expect_true(grepl("ERROR", d$info$messages$level))


})

test_that("Can flag invalid disaggs in PSNUxIM", {

  d <- list()
  d$info$tool <- "PSNUxIM"
  d$info$messages <- MessageQueue()
  d$info$cop_year <- 2024
  d$info$schema <- datapackr::cop24_psnuxim_schema
  d$info$has_error <- FALSE

  #Flag invalid disaggs
  d$data$SNUxIM <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~DataPackTarget,
    "Fish District [AYWv44mLzDN]", "TX_CURR.T", "25-34", "Female", NA, 10,
    "Dog District [BYWv44mLzDN]", "TX_CURR.T", "25-34", "Female", NA, 10,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-34", "F", NA, 10,
    "Bagel District [ZYWv44mLzDN]", "TX_CURR.T", "25-34", "F", NA, 10)

  d <- checkPSNUxIMDisaggs(d)

  expect_true(d$info$has_error)
  expect_equal(NROW(d$tests$invalid_psnuxim_disaggs), 2L)
  expect_true(grepl("invalid disagg", d$info$messages$message))
  expect_setequal(c("Cupcake District [NYWv44mLzDN]",
                    "Bagel District [ZYWv44mLzDN]"),
                  d$tests$invalid_psnuxim_disaggs$PSNU)
  #Expect an offset here due to the header row
  expect_setequal(d$tests$invalid_psnuxim_disaggs$row_number, c(17, 18))

})

test_that("Do not flag valid  disaggs in PSNUxIM", {

  d <- list()
  d$info$tool <- "PSNUxIM"
  d$info$messages <- MessageQueue()
  d$info$cop_year <- 2024
  d$info$schema <- datapackr::cop24_psnuxim_schema
  d$info$has_error <- FALSE


  #Flag invalid disaggs
  d$data$SNUxIM <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~DataPackTarget,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-34", "Female", NA, 10,
    "Bagel District [ZYWv44mLzDN]", "TX_CURR.T", "25-34", "Female", NA, 10)

  d <- checkPSNUxIMDisaggs(d)

  expect_false(d$info$has_error)

  expect_null(d$tests$invalid_psnuxim_disaggs)
})

test_that("Can identify non-numeric values in PSNUxIM", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$data$SNUxIM <- tibble::tribble(
    ~"A", ~"B", ~"C", ~"PSNU",
    "1", "foo", "bar", "Fish District",
    "-2", "2.2", "baz", "Pizza District")
  header_cols <- data.frame(indicator_code = c("PSNU"))
  d <- checkNonNumericPSNUxIMValues(d,  header_cols)
  expect_true(any(grepl("WARNING! In tab PSNUxIM: Non-numeric values!", d$info$messages$message)))
  expect_setequal(d$tests$non_numeric_psnuxim_values$columns, c("B", "C"))
  expect_equal(d$tests$non_numeric_psnuxim_values$rows[which(d$tests$non_numeric_psnuxim_values == "B")], "1")
  expect_equal(d$tests$non_numeric_psnuxim_values$rows[which(d$tests$non_numeric_psnuxim_values == "C")], "1:2")

})

test_that("Can identify and add missing dedupe columns", {
  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()
  test_data <- data.frame(foo = c(1L, 1L), bar = c(3L, 3L))
  d$data$SNUxIM <- test_data
  names(d$data$SNUxIM) <- c("foo", "bar")
  d$info$schema <- datapackr::pick_schema(2023, "PSNUxIM")
  cols_to_keep <- getColumnsToKeep(d, sheet = "PSNUxIM")
  d <- testMissingDedupeRollupColumns(d, cols_to_keep)
  expect_setequal(
    names(d$data$SNUxIM),
    c(
      "foo",
      "bar",
      "Total Deduplicated Rollup",
      "Deduplicated DSD Rollup",
      "Deduplicated TA Rollup",
      "Not PEPFAR"
    )
  )

})

test_that("Can identify negative mechanism target values", {


  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$data$SNUxIM <- tibble::tribble(
    ~"PSNU", ~"indicator_code", ~"Age", ~"Sex", ~"KeyPop", ~"9999_DSD",
    "abc123", "HTS_TST.KP.Neg.T", NA, NA, "PWID", -10,
    "abc123", "HTS_TST.KP.Pos.T", NA, NA, "PWID", 10
  )

  header_cols <- data.frame(indicator_code = c("PSNU", "indicator_code", "Age", "Sex", "KeyPop"))

  d <- testNegativeTargetValues(d,  header_cols)
  expect_true(is.data.frame(d$test$negative_IM_targets))
  expect_equal(NROW(d$tests$negative_IM_targets), 1L)
  expect_true(d$info$has_error)
  expect_true(grepl("1 cases", d$info$messages$message))
  expect_true(setequal(names(d$tests$negative_IM_targets),
                       c("PSNU", "indicator_code",
                         "Age", "Sex", "KeyPop", "mechCode_supportType", "value")))
  expect_true(all(d$tests$negative_IM_targets$value < 0))

  #Note that this function remove negative targets, but does not remove the row
  ref <- tibble::tribble(
    ~"PSNU", ~"indicator_code", ~"Age", ~"Sex", ~"KeyPop", ~"9999_DSD",
    "abc123", "HTS_TST.KP.Neg.T", NA, NA, "PWID", NA_real_,
    "abc123", "HTS_TST.KP.Pos.T", NA, NA, "PWID", 10
  )
  expect_identical(d$data$SNUxIM, ref)

})

test_that(
  "Can recalculate dedupe values",
  {
    d <- list()
    d$info$messages <- MessageQueue()
    d$info$has_error <- FALSE
    df <- tibble::tribble(
      ~ "PSNU",
      ~ "indicator_code",
      ~ "Age",
      ~ "Sex",
      ~ "KeyPop",
      ~ "1234_DSD",
      ~ "1234_TA",
      ~ "9999_DSD",
      ~ "9999_TA",
      ~ "Total Deduplicated Rollup",
      ~ "Deduplicated DSD Rollup",
      ~ "Deduplicated TA Rollup",
      "abc123",
      "HTS_TST.KP.Neg.T",
      NA,
      NA,
      "PWID",
      10,
      20,
      30,
      40,
      100,
      40,
      60
    )


  d$data$SNUxIM <- df
  d <- recalculateDedupeValues(d)
  #Simple case with DSD and TA correspoding to SUM or zero dedupe
  expect_equal(d$data$SNUxIM$`MAX - TA`, 40)
  expect_equal(d$data$SNUxIM$`MAX - DSD`, 30)
  expect_equal(d$data$SNUxIM$`TA Duplicated Rollup`, 60)
  expect_equal(d$data$SNUxIM$`DSD Duplicated Rollup`, 40)
  expect_equal(d$data$SNUxIM$`SUM - Crosswalk Total`, 100)
  expect_equal(d$data$SNUxIM$`MAX - Crosswalk Total`, 60)
  expect_equal(d$data$SNUxIM$`DSD Dedupe`, 0)
  expect_equal(d$data$SNUxIM$`TA Dedupe`, 0)
  expect_equal(d$data$SNUxIM$`Crosswalk Dedupe`, 0)

  #Mechanisms 1234 values should be deduped out in this case
  #There is no DSD-TA crosswalk dedupe here.
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "1234_TA",
    ~ "9999_DSD",
    ~ "9999_TA",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10,
    20,
    30,
    40,
    70,
    30,
    40
  )

  d$data$SNUxIM <- df
  d <- recalculateDedupeValues(d)
  expect_equal(d$data$SNUxIM$`MAX - TA`, 40)
  expect_equal(d$data$SNUxIM$`MAX - DSD`, 30)
  expect_equal(d$data$SNUxIM$`TA Duplicated Rollup`, 60)
  expect_equal(d$data$SNUxIM$`DSD Duplicated Rollup`, 40)
  expect_equal(d$data$SNUxIM$`SUM - Crosswalk Total`, 70)
  expect_equal(d$data$SNUxIM$`MAX - Crosswalk Total`, 40)
  expect_equal(d$data$SNUxIM$`DSD Dedupe`, -10)
  expect_equal(d$data$SNUxIM$`TA Dedupe`, -20)
  expect_equal(d$data$SNUxIM$`Crosswalk Dedupe`, 0)

  #Complete overlap between DSD and TA. TA should get deduped out completely
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "1234_TA",
    ~ "9999_DSD",
    ~ "9999_TA",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10,
    20,
    30,
    40,
    40,
    40,
    40
  )

  d$data$SNUxIM <- df
  d <- recalculateDedupeValues(d)
  expect_equal(d$data$SNUxIM$`MAX - TA`, 40)
  expect_equal(d$data$SNUxIM$`MAX - DSD`, 30)
  expect_equal(d$data$SNUxIM$`TA Duplicated Rollup`, 60)
  expect_equal(d$data$SNUxIM$`DSD Duplicated Rollup`, 40)
  expect_equal(d$data$SNUxIM$`SUM - Crosswalk Total`, 80)
  expect_equal(d$data$SNUxIM$`MAX - Crosswalk Total`, 40)
  expect_equal(d$data$SNUxIM$`DSD Dedupe`, 0)
  expect_equal(d$data$SNUxIM$`TA Dedupe`, -20)
  expect_equal(d$data$SNUxIM$`Crosswalk Dedupe`, -40)

})

test_that("Can check invalid dedupe values", {
  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$info$schema <- datapackr::pick_schema(2023, "PSNUxIM")
  cols_to_keep <- getColumnsToKeep(d, "PSNUxIM")
  header_cols <- getHeaderColumns(cols_to_keep, sheet)

  #In this case, the Deduplicated DSD Rollup is impossible. It must be at
  #at least 30. There is no problem with the total, since it is less
  #Than or equal to the max of all values, but it is impossible
  #to have less than 30 DSD and less than 40 TA.
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "1234_TA",
    ~ "9999_DSD",
    ~ "9999_TA",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10,
    20,
    30,
    40,
    40,
    20,
    20
  )

  d$data$SNUxIM <- df
  d <- recalculateDedupeValues(d)

  expect_equal(d$data$SNUxIM$`MAX - TA`, 40)
  expect_equal(d$data$SNUxIM$`MAX - DSD`, 30)
  expect_equal(d$data$SNUxIM$`TA Duplicated Rollup`, 60)
  expect_equal(d$data$SNUxIM$`DSD Duplicated Rollup`, 40)
  expect_equal(d$data$SNUxIM$`SUM - Crosswalk Total`, 40)
  expect_equal(d$data$SNUxIM$`MAX - Crosswalk Total`, 20)
  expect_equal(d$data$SNUxIM$`DSD Dedupe`, -20)
  expect_equal(d$data$SNUxIM$`TA Dedupe`, -40)
  expect_equal(d$data$SNUxIM$`Crosswalk Dedupe`, 0)

  d <- testInvalidDedupeValues(d, header_cols)

  expect_true(d$tests$dedupes_outside_range$`issues.Deduplicated DSD Rollup`)
  expect_true(d$tests$dedupes_outside_range$`issues.Deduplicated TA Rollup`)
  expect_false(d$tests$dedupes_outside_range$`issues.Total Deduplicated Rollup`)

})

test_that("Can recalculate initial dedupe values", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "1234_TA",
    ~ "9999_DSD",
    ~ "9999_TA",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10,
    20,
    30,
    40,
    100,
    40,
    60
  )

  d$data$SNUxIM <- df
  d <- recalculateDedupeValues(d)
  #Simple case with DSD and TA correspoding to SUM or zero dedupe
  expect_equal(d$data$SNUxIM$`MAX - TA`, 40)
  expect_equal(d$data$SNUxIM$`MAX - DSD`, 30)
  expect_equal(d$data$SNUxIM$`TA Duplicated Rollup`, 60)
  expect_equal(d$data$SNUxIM$`DSD Duplicated Rollup`, 40)
  expect_equal(d$data$SNUxIM$`SUM - Crosswalk Total`, 100)
  expect_equal(d$data$SNUxIM$`MAX - Crosswalk Total`, 60)
  expect_equal(d$data$SNUxIM$`DSD Dedupe`, 0)
  expect_equal(d$data$SNUxIM$`TA Dedupe`, 0)
  expect_equal(d$data$SNUxIM$`Crosswalk Dedupe`, 0)


})

test_that("Can identify invalid PSNUs", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$info$country_uids <- "cDGPF739ZZr"
  d$info$cop_year <- 2023

  #Looks OK, but the UID is not correct for Joburg
  d$data$SNUxIM <- tibble::tribble(
    ~"PSNU", ~"psnuid",
    "ec Oliver Tambo District Municipality [us2FGzlnk8l]", "us2FGzlnk8l",
    "gp City of Johannesburg Metropolitan Municipality [NXV5m5fAdaI]", "NXV5m5fAdaI"
  )

  d <- testInvalidPSNUs(d)
  expect_true(inherits(d$tests$invalid_psnus, "data.frame"))
  expect_setequal(names(d$tests$invalid_psnus), c("PSNU"))
  expect_equal(NROW(d$tests$invalid_psnus), 1L)

})

test_that("Can recalculate final dedupe values", {
  ## If only 1 DSD mechanism or only 1 TA mechanism (1 mech total):
  ##   - Do not import any dedupes (Dedupe = NA_real_)

  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "psnuid",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "1234_TA",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10, #1234_DSD
    20, #1234_TA
    30, #Total
    30, #DSD Total
    NA #TA Total
  )

  d$info$schema <- datapackr::pick_schema(2023, "PSNUxIM")
  cols_to_keep <- getColumnsToKeep(d, "PSNUxIM")
  header_cols <- getHeaderColumns(cols_to_keep, sheet)
  d$data$SNUxIM <- df

  d <- recalculateDedupeValues(d)

  d$data$SNUxIM %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(c(header_cols$indicator_code, "psnuid"))) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mechCode_supportType, value) %>%
    tidyr::drop_na(value)

  d <- calculateFinalDedupeValues(d, header_cols)

  expect_true(!all(grepl("^(TA Dedupe)$", d$data$SNUxIM$mechCode_supportType)))
  expect_true(!all(grepl("^(DSD Dedupe)$", d$data$SNUxIM$mechCode_supportType)))
  expect_true(d$data$SNUxIM[[which(d$data$SNUxIM$mechCode_supportType == "Crosswalk Dedupe"), "value"]] == 0)


  ## If only 1 DSD mech and only 1 TA mech (2 mechs total):
  ##   - Import Crosswalk Dedupe, whether 0 or <0
  ##   - Do not import any DSD or TA Dedupes (NA_real_)

  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "psnuid",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "9999_TA",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10, #1234_DSD
    40, #9999_TA
    50, #Total
    10, #DSD Total
    40 #TA Total
  )

  d$info$schema <- datapackr::pick_schema(2023, "PSNUxIM")
  cols_to_keep <- getColumnsToKeep(d, "PSNUxIM")
  header_cols <- getHeaderColumns(cols_to_keep, sheet)
  d$data$SNUxIM <- df

  d <- recalculateDedupeValues(d)

  d$data$SNUxIM %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(c(header_cols$indicator_code, "psnuid"))) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mechCode_supportType, value) %>%
    tidyr::drop_na(value)

  d <- calculateFinalDedupeValues(d, header_cols)
  expect_true(!all(grepl("^(TA Dedupe)$", d$data$SNUxIM$mechCode_supportType)))
  expect_true(!all(grepl("^(DSD Dedupe)$", d$data$SNUxIM$mechCode_supportType)))
  expect_true(d$data$SNUxIM[[which(d$data$SNUxIM$mechCode_supportType == "Crosswalk Dedupe"), "value"]] == 0)



  ## If >1 DSD mech, but no TA mechs (or vice versa):
  ##   - Import DSD or TA dedupes, whether 0 or <0 (if NA -> 0)
  ##   - Do not import any Crosswalk dedupes
  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "psnuid",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "9999_DSD",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10, #1234_DSD
    30, #9999_DSD
    30, #Total
    30, #DSD Total
    NA #TA Total
  )

  d$info$schema <- datapackr::pick_schema(2023, "PSNUxIM")
  cols_to_keep <- getColumnsToKeep(d, "PSNUxIM")
  header_cols <- getHeaderColumns(cols_to_keep, sheet)
  d$data$SNUxIM <- df

  d <- recalculateDedupeValues(d)

  d$data$SNUxIM %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(c(header_cols$indicator_code, "psnuid"))) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mechCode_supportType, value) %>%
    tidyr::drop_na(value)

  d <- calculateFinalDedupeValues(d, header_cols)
  expect_true(!all(grepl("^(TA Dedupe)$", d$data$SNUxIM$mechCode_supportType)))
  expect_true(!all(grepl("^(Crosswalk Dedupe)$", d$data$SNUxIM$mechCode_supportType)))
  expect_true(d$data$SNUxIM[[which(d$data$SNUxIM$mechCode_supportType == "DSD Dedupe"), "value"]] == -10)


  ## If >1 DSD mech and >1 TA mech:
  ##   - Import all dedupes, whether 0 or <0 (if NA -> 0)


  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  df <- tibble::tribble(
    ~ "PSNU",
    ~ "psnuid",
    ~ "indicator_code",
    ~ "Age",
    ~ "Sex",
    ~ "KeyPop",
    ~ "1234_DSD",
    ~ "1234_TA",
    ~ "9999_DSD",
    ~ "9999_TA",
    ~ "Total Deduplicated Rollup",
    ~ "Deduplicated DSD Rollup",
    ~ "Deduplicated TA Rollup",
    "abc123",
    "abc123",
    "HTS_TST.KP.Neg.T",
    NA,
    NA,
    "PWID",
    10, #1234_DSD
    20, #1234_TA
    30, #9999_DSD
    40, #9999_TA
    40, #Total
    40, #DSD Total
    40 #TA Total
  )

  d$info$schema <- datapackr::pick_schema(2023, "PSNUxIM")
  cols_to_keep <- getColumnsToKeep(d, "PSNUxIM")
  header_cols <- getHeaderColumns(cols_to_keep, sheet)
  d$data$SNUxIM <- df

  d <- recalculateDedupeValues(d)

  d$data$SNUxIM %<>%
    tidyr::gather(key = "mechCode_supportType",
                  value = "value",
                  -tidyselect::all_of(c(header_cols$indicator_code, "psnuid"))) %>%
    dplyr::select(dplyr::all_of(header_cols$indicator_code), psnuid,
                  mechCode_supportType, value) %>%
    tidyr::drop_na(value)

  d <- calculateFinalDedupeValues(d, header_cols)
  expect_true(d$data$SNUxIM[[which(d$data$SNUxIM$mechCode_supportType == "TA Dedupe"), "value"]] == -20)
  expect_true(d$data$SNUxIM[[which(d$data$SNUxIM$mechCode_supportType == "DSD Dedupe"), "value"]] == 0)
  expect_true(d$data$SNUxIM[[which(d$data$SNUxIM$mechCode_supportType == "Crosswalk Dedupe"), "value"]] == -40) })


test_that("Can test and round decimal values in PSNUxIM tab", {
  d <- list()
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE
  d$data$SNUxIM <- tibble::tribble(
    ~"mechCode_supportType", ~"value",
    "12345_DSD", 1,
    "6789_TA", 1.45
  )

  d <- testRoundDecimalValues(d)

  expect_equal(NROW(d$tests$decimals), 1L)
  expect_equal(d$tests$decimals$mechCode_supportType, "6789_TA")
  expect_equal(d$tests$decimals$value, 1.45)

  expect_false(d$info$has_error)

  expect_true(grepl("DECIMAL VALUES", d$info$messages$message))


  after_rounding <-
    tibble::tribble(
      ~"mechCode_supportType", ~"value",
      "12345_DSD", 1,
      "6789_TA", 1
    )

  expect_identical(d$data$SNUxIM, after_rounding)

})
