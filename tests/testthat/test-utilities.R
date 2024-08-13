context("Test utility functions")

# swapColumns ----
test_that("Testing swapping of columns...", {

  # create input data frames
  df1 <-
    data.frame(
      w = c(0, 9, 8),
      x = c(1, 2, 3),
      y = c(4, 5, 6)
    )

  df2 <-
    data.frame(
      x = c(9, 9, 9),
      y = c(1, 1, 1),
      z = c(6, 6, 6)
    )

  # create output data frame
  df_output <-
    data.frame(
      w = c(0, 9, 8),
      x = c(9, 9, 9),
      y = c(1, 1, 1)
    )

  # perform test when not tibble
  res <- swapColumns(df1, df2)
  expect_equal(res, df_output)
  rm(res)

  # perform test when  tibble
  res <- swapColumns(df1 %>% tibble(), df2 %>% tibble())
  expect_equal(res, df_output %>% tibble())

})


test_that("Get a map of data elements and category options", {
  expect_error(getMapDataPack_DATIM_DEs_COCs("foo"))
  expect_error(getMapDataPack_DATIM_DEs_COCs(1776))


  de_map_names <- c("indicator_code", "col_type", "value_type",
                    "categoryoption_specified", "valid_ages.name",
                    "valid_ages.id", "valid_sexes.name", "valid_sexes.id",
                    "valid_kps.name", "valid_kps.id", "FY", "period",
                    "categoryOptions.ids", "dataelementuid", "hts_modality",
                    "period_dataset", "dataelementname", "categoryoptioncomboname",
                    "categoryoptioncombouid", "targets_results", "dataset", "resultstatus",
                    "resultstatus_inclusive", "disagg_type", "technical_area", "top_level",
                    "support_type", "numerator_denominator")


  de_map <- getMapDataPack_DATIM_DEs_COCs(2023)
  expect_named(de_map, de_map_names)
  de_map <- getMapDataPack_DATIM_DEs_COCs(2024)
  expect_named(de_map, de_map_names)
  expect_named(datapackr::cop23_map_DataPack_DATIM_DEs_COCs, de_map_names)
  expect_named(datapackr::cop24_map_DataPack_DATIM_DEs_COCs, de_map_names)

  #Be sure that there are no T2 indicator codes in the map
  expect_true(!any(grepl("^T2", de_map$indicator_code)))

  expect_true(is.data.frame(de_map))

})


test_that("Can extract a UID", {
  uid_we_want <- "A1234bcDE56"
  string_w_uid <- paste0("This is a UID [", uid_we_want, "]")
  uid <- extract_uid(string_w_uid)

  expect_true(uid == uid_we_want)

  uid_1 <- uid_we_want
  uid_2 <- "B678cdEfg91"
  string_w_uids <- paste0("[", uid_1, ".", uid_2, "]")
  uids <- extract_uid_all(string_w_uids)

  expect_true(setequal(uids, c(uid_1, uid_2)))

})

test_that("Testing can merge data packs...", {

  # create first data pack ----
  d1 <- list()
  d1$info$datapack_name <- "Ehtiopia"
  d1$info$tool <- "Data Pack"
  d1$info$messages <- MessageQueue()
  d1$info$has_error <- FALSE

  # data elements
  d1$datim$UndistributedMER <-
    tibble::tribble(
      ~dataElement, ~period, ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
      "agoURWZyPpn", "2024Oct", "ARBd3LRy8K3", "BYmlmGMcCWx", "HllvX50cXC0", 1.3,
      "agoURWZyPpn", "2024Oct", "ARBd3LRy8K3", "u88hOHhmLuF", "HllvX50cXC0", 1
    )

  d1$datim$prioritizations <-
    tibble::tribble(
      ~dataElement, ~period, ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
      "r4zbW3owX9n", "2024Oct", "aBzwAKpuSwJ", "HllvX50cXC0", "HllvX50cXC0", 7,
      "r4zbW3owX9n", "2024Oct", "aj1PTnemHwu", "HllvX50cXC0", "HllvX50cXC0", 7
    )

  d1$data$MER <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Abergele [#SNU] [CzLHEQJw9iM]", "CzLHEQJw9iM", "Cascade", "HTS_INDEX_FAC.New.Neg.T", "01-04", "Female", "NA", 45,
      "Abergele [#SNU] [CzLHEQJw9iM]", "CzLHEQJw9iM", "Cascade", "HTS_INDEX_FAC.New.Neg.T", "01-04", "Male", "NA", 45
    )

  d1$data$SUBNAT_IMPATT <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Abergele [#SNU] [CzLHEQJw9iM]", "CzLHEQJw9iM", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 7,
      "Abergele [#SNU] [CzLHEQJw9iM]", "CzLHEQJw9iM", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 7
    )

  # tests
  d1$tests$PSNUxIM_rounding_diffs <-
    tibble::tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~PSNUxIM_value, ~DataPack_value, ~diff,
      "Addis Ketema Woreda 1 [#SNU] [Dl0yK0OhftZ]", "HTS_INDEX_FAC", "25-29", "Female", NA, "Dl0y", 15, 14, 1
    )

  # empty invalid psnus
  d1$tests$invalid_psnus <- character(0)

  # add messages
  lvl <- "ERROR"
  msg <-
    paste0(
      lvl, "! In tab Prioritization",
      ": INVALID PRIORITIZATIONS: The following PSNUs have been assigned",
      " invalid or blank prioritizations")
  attr(d1$info$messages, "test_name") <- "Invalid prioritizations"
  d1$info$messages <- appendMessage(d1$info$messages, msg, lvl)
  d1$info$has_error <- TRUE


  # create second data pack ----
  d2 <- list()
  d2$info$datapack_name <- "Ehtiopia"
  d2$info$tool <- "Data Pack"
  d2$info$messages <- MessageQueue()
  d2$info$has_error <- FALSE

  # data elements
  d2$datim$UndistributedMER <-
    tibble::tribble(
      ~dataElement, ~period, ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
      "agoURWZyPpn", "2024Oct", "ARBd3LRy8K3", "BYmlmGMcCWx", "HllvX50cXC0", 1,
      "agoURWZyPpn", "2024Oct", "ARBd3LRy8K3", "u88hOHhmLuF", "HllvX50cXC0", 1
    )

  d2$datim$prioritizations <-
    tibble::tribble(
      ~dataElement, ~period, ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
      "r4zbW3owX9n", "2024Oct", "aBzwAKpuSwJ", "HllvX50cXC0", "HllvX50cXC0", 7,
      "r4zbW3owX9n", "2024Oct", "aj1PTnemHwu", "HllvX50cXC0", "HllvX50cXC0", 7
    )

  d2$data$MER <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Abergele [#SNU] [CzLHEQJw9iM]", "CzLHEQJw9iM", "Cascade", "HTS_INDEX_FAC.New.Neg.T", "01-04", "Female", "NA", 45,
      "Abergele [#SNU] [CzLHEQJw9iM]", "CzLHEQJw9iM", "Cascade", "HTS_INDEX_FAC.New.Neg.T", "01-04", "Male", "NA", 45
    )

  d2$data$SUBNAT_IMPATT <-
    tibble::tribble(
      ~PSNU, ~psnuid, ~sheet_name, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~value,
      "Abergele [#SNU] [CzLHEQJw9iM]", "CzLHEQJw9iM", "Prioritization", "IMPATT.PRIORITY_SNU.T", NA, NA, NA, 7
    )

  # tests
  d2$tests$PSNUxIM_rounding_diffs <-
    tibble::tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~PSNUxIM_value, ~DataPack_value, ~diff,
      "Addis Ketema Woreda 1 [#SNU] [Dl0yK0OhftZ]", "HTS_INDEX_FAC", "25-29", "Female", NA, "Dl0y", 15, 14, 1
    )

  d2$tests$invalid_mech_headers <-
    tibble::tibble()

  d2$info$messages <- datapackr::MessageQueue()
  lvl <- "WARNING"
  msg <-
    paste0(
      lvl,
      "! In tab Cascade",
      ": DECIMAL VALUES found in the following columns that should have only")
  attr(d2$info$messages, "test_name") <- "Decimal values"
  d2$info$messages <- appendMessage(d2$info$messages, msg, lvl)


  # testing ----

  dp_merged <- mergeDatapack(d1, d2)

  # test binding
  testthat::expect_equal(NROW(dp_merged$datim$UndistributedMER), 4)
  testthat::expect_equal(NROW(dp_merged$datim$prioritizations), 4)
  testthat::expect_equal(NROW(dp_merged$data$MER), 4)
  testthat::expect_equal(NROW(dp_merged$data$SUBNAT_IMPATT), 3)
  testthat::expect_equal(NROW(dp_merged$tests$PSNUxIM_rounding_diffs), 2)
  testthat::expect_true(
    unique(names(dp_merged$tests) %in% c("PSNUxIM_rounding_diffs", "invalid_psnus", "invalid_mech_headers"))
  )
  testthat::expect_equal(NROW(dp_merged$info$messages), 2)


  # test error out from different data pack
  d2$info$datapack_name <- "Cameroon"
  testthat::expect_error(mergeDatapack(d1, d2), "We cannot merge those two tools.")

})



test_that("Can calculate a max by row for a data frame", {

  test_data <- tibble::tribble(
    ~`12345_DSD`, ~`12555_DSD`, ~`34567_DSD`,
    1, 2, 3,
    2, 1, 5
  )

  test_result <- rowMax(test_data, "row_max", "^12")
  expect_named(test_result, c(names(test_data), "row_max"), ignore.order = TRUE)
  expect_true(test_result$row_max[1] == 2)
  expect_true(test_result$row_max[2] == 2)

  test_result <- rowMax(test_data, "row_max", "foo")
  expect_named(test_result, c(names(test_data), "row_max"), ignore.order = TRUE)
  expect_true(all(is.na(test_result$row_max)))
})

test_that("Can get an operating unit from country UIDs", {
  #Use Angola's UID
  expect_warning(getOUFromCountryUIDs("XOivy2uDpMF"))
  expect_error(getOUFromCountryUIDs(cop_year = 2024))
  expect_error(getOUFromCountryUIDs("foo", 2024))
  expect_error(getOUFromCountryUIDs("XOivy2uDpMF", 1776))
  expect_error(getOUFromCountryUIDs("XOivy2uDpMF", c(2023, 2024)))

  df <- getOUFromCountryUIDs("XOivy2uDpMF", 2024)
  expect_named(df, c("ou", "ou_uid"))
  expect_true(NROW(df) == 1)
  expect_true(is_uidish(df$ou_uid))

  #Kazakhstan and Kygrystan
  df <- getOUFromCountryUIDs(country_uids = c("xVvOdyoS7wi", "vm58KTm9wvy"), cop_year = 2023)
  expect_equal(df$ou, "Asia Region")

  #Kazakhstan and Angola
  expect_error(getOUFromCountryUIDs(country_uids = c("XOivy2uDpMF", "vm58KTm9wvy"), cop_year = 2024))

})
