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