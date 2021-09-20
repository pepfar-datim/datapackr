context("pull-code-lists")

with_mock_api({
  test_that("We can fetch a code list from DATIM", {
    test_dataset <-  pullDATIMCodeList("YfZot37BbTm",
                                    d2_session = training)
    expect_type(test_dataset,"list")
    test_dataset_names <-  c("dataset",
                   "dataelement",
                   "shortname",
                   "code",
                   "dataelementuid",
                   "dataelementdesc",
                   "categoryoptioncombo",
                   "categoryoptioncombocode","categoryoptioncombouid")
    expect_true(setequal(test_dataset_names,names(test_dataset)))

    #Expect error on a faulty dataset UID
    expect_error(pullDATIMCodeList("foo"))
  })
})

with_mock_api({
  test_that("We can fetch a COP21 indicator list from DATIM", {
    #Hard code the year, as this test may break around Ocotber
    test_dataset <-  pull_COPindicators(cop_year = 2021,
                                     d2_session = training)
    expect_type(test_dataset,"list")
    expect_identical(class(test_dataset),"data.frame")
    expect_true(NROW(test_dataset) > 0)
    test_dataset_names <-  c("name","id","denominatorDescription",
    "numeratorDescription","numerator","denominator",
                           "code","indicatorType.name","indicatorType.id")
    expect_true(setequal(test_dataset_names,names(test_dataset)))

  })
})

with_mock_api({
  test_that("We can fetch a map of technical areas from DATIM", {
    test_dataset <-  getTechArea(d2_session = training)
    expect_type(test_dataset,"list")
    expect_equal(length(test_dataset),2)
    expect_setequal(names(test_dataset),c("dataElement","tech_area"))
    expect_true(length(test_dataset$dataElement) > 0)
    expect_true(length(test_dataset$tech_area) > 0)
    expect_true(length(test_dataset$dataElement) == length(test_dataset$tech_area))

  })
})

with_mock_api({
  test_that("We can fetch a map of HTS modalities from DATIM", {

    test_dataset <-  getHTSModality(d2_session = training)
    expect_type(test_dataset,"list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(names(test_dataset),c("dataElement","hts_modality"))
    expect_true(all(grepl("FY\\d{2}" , test_dataset$hts_modality) == FALSE))

  })
})


with_mock_api({
  test_that("We can get dataset assignment levels from DATIM", {

    test_dataset <-  getIMPATTLevels(d2_session = training)
    expect_type(test_dataset,"list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(names(test_dataset),c("operating_unit","country_name","country_uid","iso3","iso4","country","facility","community","prioritization"))

  })
})

with_mock_api({
  test_that("We can get valid category options from DATIM", {

    test_dataset <-  getValidCOs(d2_session = training)
    expect_type(test_dataset,"list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(names(test_dataset),c("data_element.name","data_element.id","category_combo.name","category_combo.id","grp","category_option.name","category_option.id"))

  })
})

with_mock_api({
  test_that("We can get a list of PSNUs from DATIM", {

    test_dataset <-  getPSNUs(country_uids = "qllxzIjjurr",include_mil = FALSE,
                           include_DREAMS = TRUE,
                           d2_session = training)
    expect_type(test_dataset,"list")
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
    expect_equal(class(test_dataset$ancestors),"list")
    expect_equal(class(test_dataset$organisationUnitGroups),"list")

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

test_that("We can get a list of dataset UIDs based on the fiscal year",{
  expect_error(getDatasetUids("foo"))
  test_dataset <-  getDatasetUids(2021)
  expect_type(test_dataset, "character")
  expect_true(length(test_dataset) > 0)
  expect_true(all(unlist(lapply(test_dataset,is_uidish))))
})

with_mock_api({
  test_that("We can get a full code list", {

    test_dataset  <-   pullFullCodeList(FY = 2021, d2_session = training)
    expect_type(test_dataset, "list")
    expect_setequal(names(test_dataset), c("dataelement", "dataelementuid", "categoryoptioncombo","categoryoptioncombouid","FY"))
    skip("FY22 code lists are not working?")
    test_dataset  <-   pullFullCodeList(FY = 2022, d2_session = training)
    })
})
