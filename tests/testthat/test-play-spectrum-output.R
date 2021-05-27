context("Test Spectrum data generation") 

with_mock_api({
  test_that("We can generate play Spectrum data", {
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset <- create_play_spectrum_output(country_uids = "qllxzIjjurr",
                                                cop_year = 2021)
    
    expect_named(
      test_dataset,
      c(
        "psnu",
        "psnu_uid",
        "area_id",
        "indicator_code",
        "dataelementuid",
        "age",
        "age_uid",
        "sex",
        "sex_uid",
        "calendar_quarter",
        "value",
        "age_sex_rse",
        "district_rse"
      )
    )
  })
})