
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
  expect_equal(NROW(dp_merged$datim$UndistributedMER), 4)
  expect_equal(NROW(dp_merged$datim$prioritizations), 4)
  expect_equal(NROW(dp_merged$data$MER), 4)
  expect_equal(NROW(dp_merged$data$SUBNAT_IMPATT), 3)
  expect_equal(NROW(dp_merged$tests$PSNUxIM_rounding_diffs), 2)
  expect_true(
    unique(names(dp_merged$tests) %in% c("PSNUxIM_rounding_diffs", "invalid_psnus", "invalid_mech_headers"))
  )
  expect_equal(NROW(dp_merged$info$messages), 2)
  expect_true(all(dp_merged$messages$message$tool == "Data Pack"))


  # test error out from different data pack
  d2$info$datapack_name <- "Cameroon"
  testthat::expect_error(mergeDatapack(d1, d2), "We cannot merge those two tools.")

})

test_that("Testing can merge a datapack with a PSNUxIM...", {
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

  #Create mock PSNUxIM
  d2 <- list()
  d2$info$datapack_name <- "Ehtiopia"
  d2$info$tool <- "PSNUxIM"
  d2$info$messages <- MessageQueue()
  d2$info$has_error <- FALSE

  d2$tests$psnuxim_missing_rs_fxs <- tibble::tribble(
    ~col, ~row, ~formula,~col_letter,
    4,10, "=(B10*100)/C10", "D"
  )

  d2$data$SNUxIM <-
    tibble::tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~psnuid, ~mech_code, ~support_type, ~value,
      "Addis Ketema Woreda 1 [#SNU] [Dl0yK0OhftZ]", "CXCA_SCRN.T", "25-34", "Female", NA_character_, "Dl0yK0OhftZ", "12345", "DSD", 3896)


  attr(d2$tests$psnuxim_missing_rs_fxs, "test_name") <- "Missing PSNUxIM R.S. Formulas"

    warning_msg <-
      paste0(
        "WARNING! In tab PSNUxIM: MISSING FORMULAS ON RIGHT SIDE.",
        " Make sure all formulas in the far right section of your PSNUxIM tab",
        " (section titled 'Target Values') are completely copied to the bottom",
        " of your data. The following columns are implicated. -> \n\t",
        paste(sort(unique(d2$tests$psnuxim_missing_rs_fxs$col_letter)), collapse = ", "),
        "\n")

    d2$info$messages <- appendMessage(d2$info$messages, warning_msg, "WARNING")


    # testing ----

    dp_merged <- mergeDatapack(d1, d2)

    # test binding
    expect_equal(NROW(dp_merged$datim$UndistributedMER), 2)
    expect_equal(NROW(dp_merged$datim$prioritizations), 2)
    expect_equal(NROW(dp_merged$data$MER), 2)
    expect_equal(NROW(dp_merged$data$SUBNAT_IMPATT), 2)
    expect_equal(NROW(dp_merged$tests$PSNUxIM_rounding_diffs), 1)
    expect_true(unique(
      names(dp_merged$tests) %in% c(
        "PSNUxIM_rounding_diffs",
        "invalid_psnus",
        "invalid_mech_headers",
        "psnuxim_missing_rs_fxs"
      )
    ))
    expect_equal(NROW(dp_merged$info$messages), 2)
    expect_setequal(dp_merged$info$messages$tool, c("Data Pack", "PSNUxIM"))
    expect_setequal(dp_merged$info$messages$level, c("ERROR", "WARNING"))

})
