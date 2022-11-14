context("can-unpack-COP21-datapack")

d_data_targets_names <- c("PSNU", "psnuid", "sheet_name", "indicator_code", "Age", "Sex", "KeyPop", "value")
d_data_tests_types <- c("tbl_df", "tbl", "data.frame")

d <-
  datapackr::loadDataPack(
    submission_path = test_sheet("COP21_DP_random_no_psnuxim.xlsx"),
    tool = "Data Pack",
    country_uids = NULL,
    cop_year = NULL,
    load_sheets = TRUE,
    d2_session = training)

d <- unPackSheets(d, check_sheets = TRUE)

with_mock_api({
  test_that("Can unpack all Data Pack sheets", {

    expect_true(!is.null(d$data$MER))
    expect_setequal(class(d$data$MER), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d$data$MER, typeof)), c(rep("character", 7), "double"))
    expect_setequal(names(d$data$MER), d_data_targets_names)
    expect_true((NROW(d$data$MER) > 0))

    expect_true(!is.null(d$data$SUBNAT_IMPATT))
    expect_setequal(class(d$data$SUBNAT_IMPATT), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d$data$SUBNAT_IMPATT, typeof)), c(rep("character", 7), "double"))
    expect_setequal(names(d$data$SUBNAT_IMPATT), d_data_targets_names)
    expect_true((NROW(d$data$SUBNAT_IMPATT) > 0))

    # Expect there to be test information
    # The test_name attribute should not be null
    expect_true(!is.null(d$tests))
    expect_true(all(unlist(lapply(d$tests, function(x) (setequal(class(x), d_data_tests_types))))))
    all(unlist(lapply(d$tests, function(x) !is.null(attr(x, "test_name")))))
    validation_summary <- validationSummary(d)
    expect_named(validation_summary,
                 c("count", "country_name", "country_uid",
                   "ou", "ou_id", "test_name", "validation_issue_category"),
                 ignore.order = TRUE)

    # Should throw an error if the tool is an unknown type
    d$info$tool <- "FooPack"
    expect_error(d <- unPackSheets(d, check_sheets = FALSE))
  })
})

with_mock_api({
  test_that("Can pack Undistributed data for DATIM.", {

    # Package the undistributed data for DATIM
    d <- packForDATIM(d, type = "Undistributed MER")
    expect_true(!is.null(d$data$UndistributedMER))
    expect_true(NROW(d$data$UndistributedMER) > 0)
    expect_true(all(unlist(
      lapply(d$data$UndistributedMER$dataElement, is_uidish)
    )))
    expect_true(all(unlist(
      lapply(d$data$UndistributedMER$categoryOptionCombo, is_uidish)
    )))
    expect_true(all(unlist(
      lapply(d$data$UndistributedMER$period, function(x) {
        grepl("^\\d{4}Oct$", x)
      })
    )))

    expect_type(d$data$UndistributedMER$attributeOptionCombo, "character")
    expect_type(d$data$UndistributedMER$value, "double")
  })
})

with_mock_api({
  test_that("Can unpack a COP21 Datapack with PSNUxIM", {

    d <- datapackr::loadDataPack(
        submission_path = test_sheet("COP21_DP_random_no_psnuxim.xlsx"),
        tool = "Data Pack",
        country_uids = NULL,
        cop_year = NULL,
        load_sheets = TRUE,
        d2_session = training)

    d  <- unPackDataPack(d,
        d2_session = training)

    #Most of this is tested elsewhere, so only test the structure here
    expect_named(d, c("keychain", "info", "sheets", "tests", "data", "datim"), ignore.order = TRUE)



    })})
