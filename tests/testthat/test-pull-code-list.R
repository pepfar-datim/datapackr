context("pull-code-lists") 

with_mock_api({
  test_that("We can fetch a code list from DATIM", {
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset<-pullDATIMCodeList("YfZot37BbTm")
    expect_type(test_dataset,"list")
    test_dataset_names<-c("dataset",
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
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    #Hard code the year, as this test may break around Ocotber
    test_dataset<-pull_COPindicators(cop_year=2021)
    expect_type(test_dataset,"list")
    expect_identical(class(test_dataset),"data.frame")
    expect_true(NROW(test_dataset)>0)
    test_dataset_names<-c( "name","id","denominatorDescription",
    "numeratorDescription","numerator","denominator",
                           "code","indicatorType.name","indicatorType.id")
    expect_true(setequal(test_dataset_names,names(test_dataset)))
  
  })
})

with_mock_api({
  test_that("We can fetch a map of technical areas from DATIM", {
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset<-getTechArea()
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
    
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset<-getHTSModality()
    expect_type(test_dataset,"list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(names(test_dataset),c("dataElement","hts_modality"))
    expect_true(all(grepl("FY\\d{2}" , test_dataset$hts_modality) == FALSE))
    
  })
})


with_mock_api({
  test_that("We can get dataset assignment levels from DATIM", {
    
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset<-getIMPATTLevels()
    expect_type(test_dataset,"list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(names(test_dataset),c("operating_unit","country_name","country_uid","iso3","iso4","country","facility","community","prioritization"))
    
  })
})

with_mock_api({
  test_that("We can get valid category options from DATIM", {
    
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset<-getValidCOs()
    expect_type(test_dataset,"list")
    expect_true("data.frame" %in% class(test_dataset))
    expect_true(NROW(test_dataset) > 0)
    expect_setequal(names(test_dataset),c("data_element.name","data_element.id","category_combo.name","category_combo.id","grp","category_option.name","category_option.id"))
    
  })
})

with_mock_api({
  test_that("We can get a list of PSNUs from DATIM", {
    
    datimutils::loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_dataset<-getPSNUs(country_uids = "qllxzIjjurr",include_mil = FALSE, include_DREAMS = TRUE)
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


