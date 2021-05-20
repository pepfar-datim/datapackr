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