context("test-packDataPack")

test_that("Can test different combinations of model param passing", {

  # missing both model_path and model data
  d <- list()
  d$keychain$model_data_path <- NULL
  testthat::expect_error(
  packDataPack(d, model_data = NULL),
  "You have provided neither a model path nor model data to packTool, Please provide at least one!"
  )

  # have both model_path and model data
  d <- list()
  d$keychain$model_data_path <- test_sheet("datapack_model_data.rds")
  model_data <- data.frame()
  testthat::expect_error(
    packDataPack(d, model_data = model_data),
    "You have provided both a model path and model data to packTool. Please provide only one!"
  )

})

test_that("Confirm PSNUs in Data Pack output from packDataPack match what we'd expect from valid_OrgUnits", {

  # d list
  d <- list()
  d$info$cop_year <- 2022
  country_uids <- c("qllxzIjjurr")
  d$info$country_uids <- c("qllxzIjjurr")
  d$tool$wb <- openxlsx::loadWorkbook(file = test_sheet("COP21_Data_Pack_Template.xlsx"))
  d$info$schema <- tribble(
    ~sheet_name, ~data_structure, ~col, ~indicator_code, ~col_type, ~value_type, ~formula,
    "Prioritization", "normal", 1, "SNU1", "row_header", "string",
    'IF(LEFT($B\\d+,4)=\"_Mil\",\"M\",IF(SUM($C\\d+)=0,\"\",$C\\d+))',
    "Prioritization", "normal", 2, "PSNU", "row_header", "string",
    'IF(LEFT($B\\d+,4)=\"_Mil\",\"M\",IF(SUM($C\\d+)=0,\"\",$C\\d+))',
    "Prioritization", "normal", 4, "IMPATT.PRIORITY_SNU.T", "target", "integer",
    'IF(LEFT($B\\d+,4)=\"_Mil\",\"M\",IF(SUM($C\\d+)=0,\"\",$C\\d+))'
  ) %>%
    dplyr::mutate(valid_ages = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_sexes = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_kps = I(list(data.frame(id = NA, name = NA))))

  d$data$PSNUs <- datapackr::valid_OrgUnits %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    add_dp_label(.) %>%
    dplyr::arrange(dp_label) %>%
    ## Remove DSNUs
    dplyr::filter(!is.na(org_type)) %>%
    dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)

  model_data <- list()
  model_data$qllxzIjjurr <- tribble(
    ~indicator_code, ~period, ~psn_uid, ~age_option_uid, ~sex_option_uid, ~kp_option_uid, ~value,
    "AGYW_PREV.Started.R", "2019Oct", "wpg5evyl1OL", "jcGQdcpPSJP", "Z1EnpTPaUfq", NA, 36
  )

  # pack data pack
  res <- packDataPack(d = d, model_data = model_data)

  # test PSNU output from res
  valid_psnus <- datapackr::valid_OrgUnits %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    add_dp_label(.) %>%
    dplyr::arrange(dp_label) %>%
    ## Remove DSNUs
    dplyr::filter(org_type != "DSNU") %>%
    dplyr::select(PSNU = dp_label, psnu_uid = uid)

  testthat::expect_equal(res$data$PSNUs$psnu_uid, valid_psnus$psnu_uid)

  # test DSNU output when tab is AGYW


})
