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
        "DREAMS"
      )
    )
    expect_equal(class(test_dataset$ancestors), "list")
    expect_equal(class(test_dataset$organisationUnitGroups), "list")

  })
})


test_that("We can add a DataPack Label", {

  #Angola
  df <- getValidOrgUnits(2022) %>%
    dplyr::filter(ou == "Angola")
  df2 <- add_dp_label(df)
  expect_true(setequal(names(df2),  c(names(df), "dp_label")))
  expect_true(all(stringr::str_detect(df2$dp_label, "^(.+) \\[#[A-Za-z0-9]+\\] \\[[A-Za-z0-9]+\\]$")))

  #Asia region. The DP label takes a different form
  df <- getValidOrgUnits(2022) %>%
    dplyr::filter(ou == "Asia Region")
  df2 <- add_dp_label(df)
  expect_true(setequal(names(df2),  c(names(df), "dp_label")))

  matches <- stringr::str_detect(df2$dp_label, "^(.+) > (.+) \\[#[A-Za-z0-9]+\\] \\[[A-Za-z0-9]+\\]$")
  #There is an exception when a country is a PSNU
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
  test_dataset <-  getCOPDatasetUids(2021)
  expect_type(test_dataset, "character")
  expect_true(length(test_dataset) > 0)
  expect_true(all(unlist(lapply(test_dataset, is_uidish))))
})

with_mock_api({
  test_that("We can get a full code list", {

    expect_error(getCodeList(cop_year = 2021, datasets = "foo"))
    expect_error(getCodeList())
    expect_error(getCodeList(1999))
    expect_error(getCodeList(cop_year = 2021, datastreams = c("mer_targets", "foo")))

    test_dataset <- getCodeList(2021, d2_session = training)
    expect_type(test_dataset, "list")
    expect_setequal(names(test_dataset), c("dataelement", "dataelementuid",
    "categoryoptioncombo", "categoryoptioncombouid", "FY"))
    expect_true(all(is_uidish(test_dataset$dataelementuid)))
    expect_true(all(is_uidish(test_dataset$categoryoptioncombouid)))
    expect_true(all(test_dataset$FY == "2022"))

    test_dataset <- getCodeList(2022, d2_session = training)
    expect_type(test_dataset, "list")
    expect_setequal(names(test_dataset), c("dataelement", "dataelementuid",
                                           "categoryoptioncombo", "categoryoptioncombouid", "FY"))
    expect_true(all(is_uidish(test_dataset$dataelementuid)))
    expect_true(all(is_uidish(test_dataset$categoryoptioncombouid)))
    expect_true(all(test_dataset$FY == "2023"))
    })
})
