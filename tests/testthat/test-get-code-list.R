context("get-code-lists")

with_mock_api({
  test_that("We can get a list of PSNUs from DATIM", {

    test_dataset <-
      getDataPackOrgUnits(
        include_mil = FALSE,
        include_DREAMS = TRUE,
        use_cache = FALSE,
        d2_session = training)
    expect_type(test_dataset, "list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(
      names(test_dataset),
      c("name",
        "uid",
        "org_type",
        "ou",
        "ou_uid",
        "country_name",
        "country_uid",
        "snu1",
        "snu1_uid",
        "lastUpdated",
        "ancestors",
        "organisationUnitGroups",
        "DREAMS",
        "TSNU",
        "HISTORIC_PSNU"
      )
    )
    expect_equal(class(test_dataset$ancestors), "list")
    expect_equal(class(test_dataset$organisationUnitGroups), "list")

  })
})


test_that("We can add a DataPack Label", {


  #Test for 2023. The label should consist of the PSNU name, followed by a
  #space, and then the PSNU UID in square brackets
  df <- getValidOrgUnits(2023) %>%
    dplyr::filter(ou == "Angola")
  df2 <- add_dp_label(df, 2023)
  expect_true(setequal(names(df2),  c(names(df), "dp_label")))
  expect_true(all(stringr::str_detect(df2$dp_label, "^(.+) \\[[A-Za-z0-9]+\\]$")))


  #The labels for PSNUs in regions take a different form.
  #It should be the name of the
  #country, followed by a space, then a ">", then a space, then the name of the
  #region, followed by a space, then the PSNU UID in square brackets.

  df <- getValidOrgUnits(2023) %>%
    dplyr::filter(ou == "Asia Region")
  df2 <- add_dp_label(df, 2023)
  expect_true(setequal(names(df2),  c(names(df), "dp_label")))

  matches <- stringr::str_detect(df2$dp_label, "^(.+) > (.+) \\[[A-Za-z0-9]+\\]$")
  #There is an exception when a country is a PSNU. In this case, the DP label
  #should be the name of the country, followed by a space, then the country UID
  #in square brackets
  psnu_countries <- df2$country_uid == df2$uid
  expect_true(all(matches | psnu_countries))

})

with_mock_api({
  test_that("We can get a map of COCs to COs", {

    test_dataset  <-   map_COCs_to_COs(d2_session = training)
    expect_type(test_dataset, "list")
    expect_setequal(names(test_dataset), c("name", "id", "categoryOptions"))
    expect_equal(class(test_dataset$name), "character")
    expect_equal(class(test_dataset$id), "character")
    expect_true(all(lapply(test_dataset$id, nchar) == 11))
    expect_equal(class(test_dataset$categoryOptions), "list")
    expect_true(all(unlist(lapply(test_dataset$categoryOptions, function(x) {
      all(names(x) %in% c("name", "id"))
    }))))
  })
})

with_mock_api({
  test_that("We can get a map of Cs to COs", {

    test_dataset  <-   map_Cs_to_COs(d2_session = training)
    expect_type(test_dataset, "list")
    expect_setequal(names(test_dataset), c("categoryoptiongroup", "categoryoption", "categoryoptionuid"))
  })
})

test_that("We can get a list of dataset UIDs based on the fiscal year", {
  expect_error(suppressWarnings(getCOPDatasetUids("foo")))
  test_dataset <-  getCOPDatasetUids(2024)
  expect_type(test_dataset, "character")
  expect_true(length(test_dataset) > 0)
  expect_true(all(unlist(lapply(test_dataset, is_uidish))))
})

with_mock_api({
  test_that("We can get a full code list", {

    expect_error(getCodeList(cop_year = 2024, datasets = "foo"))
    expect_error(getCodeList(1999))
    expect_error(getCodeList(cop_year = 2024, datastreams = c("mer_targets", "foo")))

    test_dataset <- getCodeList(2024, d2_session = training)
    expect_type(test_dataset, "list")
    expect_setequal(names(test_dataset), c("dataelement", "dataelementuid",
    "categoryoptioncombo", "categoryoptioncombouid", "FY"))
    expect_true(all(is_uidish(test_dataset$dataelementuid)))
    expect_true(all(is_uidish(test_dataset$categoryoptioncombouid)))
    expect_true(all(test_dataset$FY == "2025"))

    })
})
