context("Check DATIM comparison functions")


with_mock_api({
  test_that("Can compare a fresh DataPack", {

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
                 c("keychain", "info",  "sheets", "data", "datim"),
                 ignore.order = TRUE)

    compare <- compareData_DatapackVsDatim(d, d2_session = training)

    expect_named(compare, c("psnu_x_im", "psnu", "updates", "deletes"))

    diff_names <- c(
      "psnu",
      "data_element",
      "disagg",
      "attributeOptionCombo",
      "datapack_value",
      "datim_value",
      "difference",
      "effect"
    )

    expect_named(
      compare$psnu_x_im,
      diff_names
    )

    #In this case, expect no DATIM values
    expect_true(all(is.na(compare$psnu_x_im$datim_value)))
    #Values should be positive here, since the sign
    #is directed by the datapack values. Negative differences
    #indicate lower values.
    expect_true(all(compare$psnu_x_im$difference > 0))

    #Should be no mechanisms information here
    diff_names <- c(
      "psnu",
      "data_element",
      "disagg",
      "datapack_value",
      "datim_value",
      "difference"
    )

    expect_named(
      compare$psnu,
      diff_names
    )

    #There is nothing in DATIM, so nothing to delete
    expect_true(NROW(compare$deletes) == 0)

    expect_true(NROW(compare$updates) > 0)
    expect_true(all(is_uidish(compare$updates$dataElement)))
    expect_true(all(is_uidish(compare$updates$orgUnit)))


    #Adjust this test once we do not have mechanism codes in the payload
    expect_true(all(stringr::str_detect(compare$updates$categoryOptionCombo, "[[:alpha:]][[:alnum:]]{10}") |
      compare$updates$categoryOptionCombo == "default"))
    #Since this is undistributed data, we should only be dealing with the default mechanism
    expect_true(all(compare$updates$attributeOptionCombo  == "default"))
    expect_true(inherits(compare$updates$datapack_value,"numeric"))

    skip("Remove this after merging with DP-901")
    #For COP23, there should only be 2023Oct data
    expect_true(all(compare$updates$period  == "2023Oct"))

  })
})
