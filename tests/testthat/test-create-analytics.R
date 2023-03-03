context("Analytics creation tests")


with_mock_api({
  test_that("We can create analytics for a COP23 Data Pack", {

    d <-
      loadDataPack(
        submission_path = test_sheet("COP23_sample_DataPack_Malawi.xlsx"),
        tool = "Data Pack",
        country_uids = NULL,
        cop_year = NULL,
        load_sheets = TRUE,
        d2_session = training)

    d %<>%
      unPackSheets(., check_sheets = FALSE) %>%
      packForDATIM(., type = "Undistributed MER") %>%
      packForDATIM(., type = "SUBNAT_IMPATT")

    expect_named(d,
                 c("keychain", "info", "tests", "sheets", "data", "datim"),
                 ignore.order = TRUE)

    d %<>% createAnalytics(d2_session = training)

    # "upload_timestamp", removed form below to alleviate duplicates
    analytics_column_names <-
      c(
        "ou", "ou_uid", "country_name", "country_uid", "snu1", "snu1_uid", "psnu",
        "psnu_uid", "prioritization", "mechanism_code", "mechanism_desc", "partner_id",
        "partner_desc", "funding_agency", "fiscal_year", "dataelement_id", "dataelement_name",
        "indicator", "numerator_denominator", "support_type", "hts_modality",
        "categoryoptioncombo_id", "categoryoptioncombo_name", "age", "sex",
        "key_population", "resultstatus_specific",
        "disagg_type", "resultstatus_inclusive", "top_level",
        "target_value",
        "indicator_code"
      )
    expect_true(!is.null(d$data$analytics))
    expect_true(NROW(d$data$analytics) > 0)
    expect_named(d$data$analytics, analytics_column_names)
   numeric_columns <- c("fiscal_year", "target_value")
   foo <- sapply(d$data$analytics, typeof)
   expect_true(all(foo[!(names(d$data$analytics) %in%  numeric_columns)] == "character"))
   expect_true(all(foo[(names(d$data$analytics) %in%  numeric_columns)] == "double"))
   expect_true(all(sapply(d$data$analytics$ou_uid, is_uidish)))
   expect_true(all(sapply(d$data$analytics$country_uid, is_uidish)))
   expect_true(all(sapply(d$data$analytics$snu1_uid, is_uidish)))
   expect_true(all(sapply(d$data$analytics$psnu_uid, is_uidish)))
   expect_true(all(sapply(d$data$analytics$dataelement_id, is_uidish)))
  })
})
