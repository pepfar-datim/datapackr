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
with_mock_api({
test_that("Can detected missing right side formulas", {


  generation_list <- c("Eswatini")

  pick <- datapackr::COP21_datapacks_countries %>%
    dplyr::filter(datapack_name %in% generation_list) %>%
    dplyr::arrange(datapack_name)

  output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
  dir.create(output_folder)

  #This should produce a tool with no formulas on the right hand side.
  d <- packTool(tool = "OPU Data Pack",
                datapack_name = pick$datapack_name[1],
                country_uids = unlist(pick$country_uids[1]),
                template_path = NULL,
                cop_year = 2022,
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
  d$info$schema <- datapackr::cop22OPU_data_pack_schema
  cols_to_keep <- datapackr:::getColumnsToKeep(d, sheet = "PSNUxIM")

  #Do nothing if the columns are OK
  test_data <- tibble::tribble(
    ~`12345_DSD`, ~`45678_DSD`, ~`99999_TA`, ~"Not PEPFAR",
    1, 2, 3,4
  )

  d$data$SNUxIM <- test_data
  d <- dropInvalidMechColumns(d, cols_to_keep)

  expect_identical(d$data$SNUxIM, test_data)


  test_data <- tibble::tribble(
    ~`ABC_DSD`, ~`45678_DSD`, ~`99999_TA`, ~"Not PEPFAR",
    1, 2, 3,4
  )

  d$data$SNUxIM <- test_data
  d <- dropInvalidMechColumns(d, cols_to_keep)

  expect_identical(d$data$SNUxIM, test_data[,2:4])
  expect_identical(d$tests$invalid_mech_headers$invalid_mech_headers, "ABC_DSD")
  expect_true(d$info$has_error)
  expect_true(grepl("INVALID COLUMN HEADERS",d$info$messages$message))
  expect_true(grepl("ERROR", d$info$messages$level))


})

# test_that("Can extract original targets", {
#   d <- list()
#   d$info$tool <- "OPU Data Pack"
#   d$info$messages <- MessageQueue()
#
#   d$data$SNUxIM <- tibble::tribble(
#     ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~DataPackTarget,
#     "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-49", "F", NA, 10,
#     "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "15-24", "F", NA, 10)
#
#   original_targets <- extractOriginalTargets(d)
#   PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop,
#   value = DataPackTarget
#
#   })
