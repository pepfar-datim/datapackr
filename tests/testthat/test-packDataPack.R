
context("test-packDataPack")

test_that("Can check and read datapack model file...", {

  d <- list()

  # can read file
  d$keychain$model_data_path <- test_sheet("not_real_file.rds")
  testthat::expect_error(
    packDataPack(d),
    "Model data file could not be read!"
  )

})

test_that("Can pack data pack for export...", {

  d <- list()
  d$keychain$submission_path <- test_sheet("COP22_DataPack_unPackingChecks.xlsx")
  d$info$cop_year <- 2022
  d$info$tool <- "Data Pack"
  d$keychain$model_data_path <- test_sheet("datapack_model_data.rds")
  d$info$country_uids <- "qllxzIjjurr"

  # org units
  country_uids <- "qllxzIjjurr"

  # data for psnus
  d$data$PSNUs <- datapackr::valid_OrgUnits %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    add_dp_label(.) %>%
    dplyr::arrange(dp_label) %>%
    ## Remove DSNUs
    dplyr::filter(!is.na(org_type)) %>%
    dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)

  # load wb
  d$tool$wb <- openxlsx::loadWorkbook(d$keychain$submission_path)
  openxlsx::removeFilter(d$tool$wb, names(d$tool$wb))

  d$info$schema <-
    tribble(
      ~sheet_name, ~data_structure, ~col, ~indicator_code, ~col_type, ~value_type, ~formula,
      "Prioritization", "normal", "1", "SNU1", "row_header", "string", NA,
      "Prioritization", "normal", "2", "PSNU", "row_header", "string", NA,
      "Prioritization", "normal", "3", "IMPATT.PRIORITY_SNU.T_1", "past", "integer", NA,
      "Prioritization", "normal", "4", "IMPATT.PRIORITY_SNU.T", "target", "integer",
      'IF(LEFT($B15,4)="_Mil","M",IF(SUM($C15)=0,"",$C15))'
    ) %>%
    dplyr::mutate(valid_ages = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_sexes = I(list(data.frame(id = NA, name = NA)))) %>%
    dplyr::mutate(valid_kps = I(list(data.frame(id = NA, name = NA))))


  # can pack data
  d <- packDataPack(d)

  # how to test d$tool$wb? has been altered properly

})
