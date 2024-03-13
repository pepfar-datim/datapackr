
test_that("We can write psnuxim", {

  # TEST 1: missing model data ----
  d <- list()
  d$info$cop_year <- 2024
  d$info$tool <- "Data Pack"
  snuxim_model_data_path <- NULL
  testthat::expect_error(
    writePSNUxIM(d, snuxim_model_data_path = snuxim_model_data_path),
    "Cannot update PSNUxIM tab without model data."
  )

  # TEST 2: cop year not supported ----
  d <- list()
  d$info$cop_year <- 2027
  d$info$tool <- "Data Pack"
  snuxim_model_data_path <- "/my_snuxim_model_path/model.rds"

  testthat::expect_error(
    writePSNUxIM(d, snuxim_model_data_path = snuxim_model_data_path),
    "Packing PSNUxIM tabs is not supported for the requested COP year."
  )

  # TEST 3: flags errors for threaded comments ----
  d <- list()
  d$info$cop_year <- 2024
  d$info$tool <- "Data Pack"
  snuxim_model_data_path <- "/my_snuxim_model_path/model.rds"
  output_folder <- "/my_output_folder/"
  d$info$needs_psnuxim <- TRUE
  d$info$has_psnuxim <- TRUE
  d$info$has_comments_issue <- TRUE
  d$info$messages <- MessageQueue()

  d <- writePSNUxIM(
    d,
    snuxim_model_data_path = snuxim_model_data_path,
    output_folder = output_folder,
    append = TRUE)

  testthat::expect_equal(
    substr(d$info$messages$message, 1, 69),
    "ERROR! Cannot update PSNUxIM information in a Data Pack with Threaded"
  )

  # TEST 4: no psnuxim & we have UndistributedMER data ----
  d <- list()
  d$info$cop_year <- 2024
  d$info$tool <- "Data Pack"
  d$info$needs_psnuxim <- TRUE
  d$info$has_psnuxim <- FALSE
  d$info$has_comments_issue <- FALSE
  d$info$missing_psnuxim_combos <- FALSE
  d$info$country_uids <- "ODOymOOWyl0" # sierra leone
  d$datim$UndistributedMER <-
    tibble::tribble(
      ~dataElement, ~period, ~orgUnit, ~categoryOptionCombo, ~attributeOptionCombo, ~value,
      "BeEA8PsZ8Ky", "2024Oct", "AeZKV2y4FmE", "DFLZuSpRYKv", "HllvX50cXC0", 251,
      "BeEA8PsZ8Ky", "2024Oct", "AeZKV2y4FmE", "JURc3Uxzcr9", "HllvX50cXC0", 2,
      "BeEA8PsZ8Ky", "2024Oct", "AeZKV2y4FmE", "KcI8l7j9oeX", "HllvX50cXC0", 2
    )

  # need model data as it does not exist
  mock_sxnuxim_model <-
    list(
      "ODOymOOWyl0" =
        tibble::tribble(
          ~indicator_code, ~psnu_uid, ~mechanism_uid, ~mechanism_code, ~type,
          ~age_option_name, ~age_option_uid, ~sex_option_name, ~sex_option_uid,
          ~kp_option_name, ~kp_option_uid, ~value,
          "HTS.Index.Negative", "AeZKV2y4FmE", "IcaeKoEU4G6", "86965", "DSD",
          "1-9", "A9ddhoPWEUn", "Female", "Z1EnpTPaUfq", NA, NA, 93,
          "HTS.Index.Negative", "AeZKV2y4FmE", "IcaeKoEU4G6", "86965", "DSD",
          "1-9", "A9ddhoPWEUn", "Male", "Qn0I5FbKQOA", NA, NA, 106,
          "HTS.Index.Negative", "AeZKV2y4FmE", "IcaeKoEU4G6", "86965", "DSD",
          "10-14", "jcGQdcpPSJP", "Female", "Z1EnpTPaUfq", NA, NA, 19,
          "HTS.Index.Negative", "AeZKV2y4FmE", "IcaeKoEU4G6", "86965", "DSD",
          "10-14", "jcGQdcpPSJP", "Male", "Qn0I5FbKQOA", NA, NA, 19
        )
    )

  # lets temp store the rds model data
  tmp_mock_snuxim_model <- paste0("/tmp/COP24_SNUxIM_Model_data.rds")
  saveRDS(mock_sxnuxim_model, tmp_mock_snuxim_model)

  # temp output folder
  output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
  dir.create(output_folder)

  # try to write psnuxim
  d <- writePSNUxIM(
    d,
    snuxim_model_data_path = tmp_mock_snuxim_model,
    output_folder = output_folder,
    append = TRUE,
    d2_session = training
  )

  # Be sure we can unpack what we just wrote to a file
  # borrowed from test-packCOP22OPU.R
  d_out <- unPackTool(
    submission_path = d$info$output_file
    , d2_session = training
  )

  expect_identical(d$info$datapack_name, d_out$info$datapack_name)
  expect_setequal(names(d_out), c("keychain", "info", "data", "tests", "datim", "sheets"))
  # ....

  # TEST 5: has psnuxim and missing combos ----



})
