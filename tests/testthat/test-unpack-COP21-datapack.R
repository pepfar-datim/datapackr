context("can-unpack-COP21-datapack")

d_data_targets_names <- c("PSNU", "psnuid", "sheet_name", "indicator_code", "Age", "Sex", "KeyPop", "value")
d_data_tests_types <- c("tbl_df", "tbl", "data.frame")

with_mock_api({
  test_that("Can unpack all data pack sheets", {
    d <- datapackr::createKeychainInfo(
      submission_path = test_sheet("COP21_DP_random_no_psnuxim.xlsx"),
      tool = "Data Pack",
      country_uids = NULL,
      cop_year = NULL,
      d2_session = NULL
    )

    d <- unPackSheets(d)
    expect_true(!is.null(d$data$targets))
    expect_setequal(names(d$data$targets), d_data_targets_names)
    expect_true((NROW(d$data$targets) > 0))
    expect_setequal(class(d$data$targets), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d$data$targets, typeof)), c(rep("character", 7), "double"))
    # Expect there to be test information
    # The test_name attribute should not be null
    expect_true(!is.null(d$tests))
    expect_true(all(unlist(lapply(d$tests, function(x) (setequal(class(x), d_data_tests_types))))))
    all(unlist(lapply(d$tests, function(x) !is.null(attr(x, "test_name")))))
    validation_summary <- validationSummary(d)
    expect_named(validation_summary, c("test_name", "validation_issue_category", "count",
     "ou", "ou_id", "country_name", "country_uid"), ignore.order = TRUE)



    # Should throw an error if the tool is an unknown type
    d$info$tool <- "FooPack"
    expect_error(d <- unPackSheets(d))
  })
})

with_mock_api({
  test_that("Can unpack and separate data sets", {
    d <-
      datapackr::loadDataPack(
        submission_path = test_sheet("COP21_DP_random_no_psnuxim.xlsx"),
        tool = "Data Pack",
        country_uids = NULL,
        cop_year = NULL,
        load_sheets = TRUE,
        d2_session = training)

    d <- unPackSheets(d)
    expect_true(!is.null(d$data$targets))
    expect_setequal(names(d$data$targets), d_data_targets_names)
    expect_true((NROW(d$data$targets) > 0))
    expect_setequal(class(d$data$targets), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d$data$targets, typeof)), c(rep("character", 7), "double"))

    datasets <- separateDataSets(data = d$data$targets,
                                 cop_year = d$info$cop_year,
                                 tool = d$info$tool)
    d$data$MER <- datasets$MER
    d$data$SUBNAT_IMPATT <- datasets$SUBNAT_IMPATT
    d$data <- within(d$data, rm("targets"))
    expect_null(d$data$targets)
    expect_null(d$data$extract)
    expect_true(!is.null(d$data$MER))
    expect_setequal(class(d$data$MER), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d$data$MER, typeof)), c(rep("character", 7), "double"))
    expect_true(!is.null(d$data$SUBNAT_IMPATT))
    expect_setequal(class(d$data$SUBNAT_IMPATT), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d$data$SUBNAT_IMPATT, typeof)), c(rep("character", 7), "double"))


    # Package the undistributed data for DATIM
    d <- packForDATIM(d, type = "Undistributed MER")
    expect_true(!is.null(d$datim$UndistributedMER))
    expect_true(NROW(d$datim$UndistributedMER) > 0)
    expect_true(all(unlist(
      lapply(d$datim$UndistributedMER$dataElement, is_uidish)
    )))
    expect_true(all(unlist(
      lapply(d$datim$UndistributedMER$categoryOptionCombo, is_uidish)
    )))
    expect_true(all(unlist(
      lapply(d$datim$UndistributedMER$period, function(x) {
        grepl("^\\d{4}Oct$", x)
      })
    )))

    expect_type(d$datim$UndistributedMER$attributeOptionCombo, "character")
    expect_type(d$datim$UndistributedMER$value, "double")
  })
})
