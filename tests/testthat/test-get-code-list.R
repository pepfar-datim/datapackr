context("get-code-lists")

with_mock_api({
  test_that("We can get a list of PSNUs from DATIM", {

    test_dataset <-  getPSNUs(country_uids = "qllxzIjjurr", include_mil = FALSE,
                           include_DREAMS = TRUE,
                           d2_session = training)
    expect_type(test_dataset, "list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(
      names(test_dataset),
      c(
        "ou",
        "ou_id",
        "country_name",
        "country_uid",
        "snu1",
        "snu1_id",
        "psnu",
        "psnu_uid",
        "psnu_type",
        "ancestors",
        "organisationUnitGroups",
        "DREAMS"
      )
    )
    expect_equal(class(test_dataset$ancestors), "list")
    expect_equal(class(test_dataset$organisationUnitGroups), "list")

  })
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
  expect_error(suppressWarnings(getDatasetUids("foo")))
  test_dataset <-  getDatasetUids(2021)
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
